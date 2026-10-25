library(data.table)
library(tseries)
library(urca)

obj <- readRDS(file.path(PATHS$models, "01_aligned_data.rds"))
ret <- obj$returns
node_names <- PROJECT$nodes
Y <- as.matrix(ret[, ..node_names])

desc_one <- function(x) {
  n <- length(x); m <- mean(x); s <- sd(x)
  c(N = n, Mean = m, SD = s, Min = min(x), Q25 = quantile(x, .25),
    Median = median(x), Q75 = quantile(x, .75), Max = max(x),
    Skewness = mean((x - m)^3) / s^3,
    ExcessKurtosis = mean((x - m)^4) / s^4 - 3)
}
descriptive <- rbindlist(lapply(PROJECT$nodes, function(nm) {
  data.table(Series = nm, as.list(desc_one(ret[[nm]])))
}), fill = TRUE)
fwrite(descriptive, file.path(PATHS$tables, "descriptive_statistics.csv"))
fwrite(as.data.table(cor(Y), keep.rownames = "Receiver"),
       file.path(PATHS$tables, "correlation_matrix.csv"))

unitroot <- rbindlist(lapply(PROJECT$nodes, function(nm) {
  x <- ret[[nm]]
  adf <- adf.test(x, alternative = "stationary", k = floor((length(x)-1)^(1/3)))
  kps <- kpss.test(x, null = "Level")
  data.table(Series = nm, ADF = unname(adf$statistic), ADF_p = adf$p.value,
             KPSS = unname(kps$statistic), KPSS_p = kps$p.value)
}))
fwrite(unitroot, file.path(PATHS$tables, "stationarity_tests.csv"))

make_var_design <- function(Y, p) {
  Tn <- nrow(Y); K <- ncol(Y)
  X <- matrix(1, Tn - p, 1 + K * p)
  for (ell in seq_len(p)) X[, 1 + ((ell - 1) * K + 1):(ell * K)] <- Y[(p + 1 - ell):(Tn - ell), ]
  list(Y = Y[(p + 1):Tn, , drop = FALSE], X = X)
}

lag_table <- rbindlist(lapply(1:5, function(p) {
  z <- make_var_design(Y, p); E <- z$Y - z$X %*% qr.solve(z$X, z$Y)
  S <- crossprod(E) / nrow(E); K <- ncol(Y); n <- nrow(E)
  logdet <- as.numeric(determinant(S, logarithm = TRUE)$modulus)
  data.table(Lag = p, AIC = logdet + 2 * (K * (1 + K*p)) / n,
             BIC = logdet + log(n) * (K * (1 + K*p)) / n)
}))
fwrite(lag_table, file.path(PATHS$tables, "var_lag_selection.csv"))
if (lag_table[which.min(BIC), Lag] != PROJECT$var_lag)
  warning("Current data do not select the prespecified VAR lag by BIC.")

saveRDS(list(Y = Y, Date = ret$Date, descriptive = descriptive,
             correlation = cor(Y), unitroot = unitroot, lag_table = lag_table),
        file.path(PATHS$models, "02_diagnostics.rds"))
