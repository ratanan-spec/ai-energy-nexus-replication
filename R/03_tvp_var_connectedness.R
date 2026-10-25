library(data.table)

d <- readRDS(file.path(PATHS$models, "02_diagnostics.rds"))
Y <- d$Y; dates <- as.Date(d$Date); K <- ncol(Y); colnames(Y) <- PROJECT$nodes
p <- PROJECT$var_lag
if (p != 1L) stop("This implementation currently expects VAR(1), matching the manuscript.")
if (nrow(Y) <= PROJECT$init_window) stop("Initialization window is too large.")

X0 <- cbind(1, Y[1:(PROJECT$init_window - 1), , drop = FALSE])
Y0 <- Y[2:PROJECT$init_window, , drop = FALSE]
B <- qr.solve(X0, Y0)                         # rows: intercept and lag coefficients
E0 <- Y0 - X0 %*% B
Sigma <- crossprod(E0) / nrow(E0)
P_list <- lapply(seq_len(K), function(i) solve(crossprod(X0) + diag(1e-8, ncol(X0))) * Sigma[i, i])

stabilize <- function(A, bound) {
  rho <- max(Mod(eigen(A, only.values = TRUE)$values))
  if (!is.finite(rho)) stop("Non-finite local spectral radius.")
  if (rho >= bound) list(A = A * (bound / rho), rescaled = TRUE, rho = rho)
  else list(A = A, rescaled = FALSE, rho = rho)
}

gfevd <- function(A, Sigma, H) {
  K <- nrow(A); Phi <- vector("list", H); Phi[[1]] <- diag(K)
  if (H > 1) for (h in 2:H) Phi[[h]] <- Phi[[h-1]] %*% A
  theta <- matrix(0, K, K)
  for (i in seq_len(K)) for (j in seq_len(K)) {
    num <- sum(vapply(Phi, function(ph) (ph[i, , drop = FALSE] %*% Sigma[, j, drop = FALSE])^2,
                      numeric(1))) / Sigma[j, j]
    den <- sum(vapply(Phi, function(ph) (ph[i, , drop = FALSE] %*% Sigma %*% t(ph[i, , drop = FALSE])),
                      numeric(1)))
    theta[i, j] <- num / den
  }
  theta <- theta / rowSums(theta)
  theta
}

n_eff <- nrow(Y) - PROJECT$init_window
theta_array <- array(NA_real_, c(n_eff, K, K), dimnames = list(NULL, PROJECT$nodes, PROJECT$nodes))
tci <- numeric(n_eff); to <- from <- net <- matrix(NA_real_, n_eff, K,
                                                    dimnames = list(NULL, PROJECT$nodes))
rho_vec <- numeric(n_eff); rescaled <- logical(n_eff)

for (s in seq_len(n_eff)) {
  t <- PROJECT$init_window + s
  x <- c(1, Y[t - 1, ])
  pred <- drop(x %*% B)
  err <- Y[t, ] - pred
  for (i in seq_len(K)) {
    P_prior <- P_list[[i]] / PROJECT$lambda
    gain <- drop(P_prior %*% x) / as.numeric(1 + t(x) %*% P_prior %*% x)
    B[, i] <- B[, i] + gain * err[i]
    P_list[[i]] <- P_prior - tcrossprod(gain, x) %*% P_prior
  }
  Sigma <- PROJECT$kappa * Sigma + (1 - PROJECT$kappa) * tcrossprod(err)
  Sigma <- (Sigma + t(Sigma)) / 2 + diag(1e-10, K)
  st <- stabilize(t(B[-1, , drop = FALSE]), PROJECT$stability_bound)
  rho_vec[s] <- st$rho; rescaled[s] <- st$rescaled
  th <- gfevd(st$A, Sigma, PROJECT$fevd_horizon)
  theta_array[s, , ] <- th
  off <- th; diag(off) <- 0
  from[s, ] <- 100 * rowSums(off)
  to[s, ] <- 100 * colSums(off)
  net[s, ] <- to[s, ] - from[s, ]
  tci[s] <- 100 * sum(off) / K
}

out_dates <- dates[(PROJECT$init_window + 1):nrow(Y)]
dynamic <- data.table(Date = out_dates, TCI = tci, SpectralRadius = rho_vec, Rescaled = rescaled)
for (nm in PROJECT$nodes) {
  dynamic[, paste0("TO_", nm) := to[, nm]]
  dynamic[, paste0("FROM_", nm) := from[, nm]]
  dynamic[, paste0("NET_", nm) := net[, nm]]
}
avg_theta <- apply(theta_array, c(2, 3), mean, na.rm = TRUE)
directional <- data.table(Node = PROJECT$nodes, TO = colSums(avg_theta - diag(diag(avg_theta))) * 100,
                          FROM = rowSums(avg_theta - diag(diag(avg_theta))) * 100)
directional[, NET := TO - FROM]

fwrite(dynamic, file.path(PATHS$tables, "dynamic_connectedness.csv"))
fwrite(as.data.table(100 * avg_theta, keep.rownames = "Receiver"),
       file.path(PATHS$tables, "average_gfevd_matrix.csv"))
fwrite(directional, file.path(PATHS$tables, "directional_connectedness.csv"))
saveRDS(list(dynamic = dynamic, theta = theta_array, average_theta = avg_theta,
             directional = directional, output_dates = out_dates,
             settings = PROJECT), file.path(PATHS$models, "03_tvp_connectedness.rds"))
