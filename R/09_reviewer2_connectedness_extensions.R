library(data.table)
library(sandwich)
library(lmtest)
source("R/lib_tvp.R")

cobj <- readRDS(file.path(PATHS$models, "03_tvp_connectedness.rds"))
dat <- copy(cobj$dynamic)
ret <- fread(PATHS$processed)
ret[, Date := as.Date(Date)]
nodes <- PROJECT$nodes
dat <- merge(dat, ret[, .(Date, VIX)], by = "Date", all.x = TRUE)
dat[, PostAI := as.integer(Date >= PROJECT$ai_cutoff)]
dat[, HighVIX := as.integer(VIX >= quantile(VIX, PROJECT$vix_quantile, na.rm = TRUE))]

stationary_indices <- function(n, mean_block) {
  p <- 1 / mean_block; ii <- integer(n); ii[1] <- sample.int(n, 1)
  for (i in 2:n) ii[i] <- if (runif(1) < p) sample.int(n, 1) else (ii[i - 1] %% n) + 1L
  ii
}
contrast <- function(x, g) mean(x[g == 1]) - mean(x[g == 0])
one_sensitivity <- function(label, g, block, lag, B = PROJECT$bootstrap_B) {
  set.seed(PROJECT$seed + block * 100 + lag)
  vals <- replicate(B, { ii <- stationary_indices(nrow(dat), block); contrast(dat$TCI[ii], g[ii]) })
  fit <- lm(TCI ~ PostAI + HighVIX, data = dat)
  nw <- NeweyWest(fit, lag = lag, prewhite = FALSE, adjust = TRUE)
  ct <- coeftest(fit, vcov. = nw)
  term <- if (label == "PostAI") "PostAI" else "HighVIX"
  data.table(Contrast = label, BlockLength = block, HACLag = lag,
             Difference = contrast(dat$TCI, g), CI95_L = unname(quantile(vals, .025)),
             CI95_U = unname(quantile(vals, .975)), HAC_Estimate = ct[term, "Estimate"],
             HAC_p = ct[term, "Pr(>|t|)"])
}
grid <- CJ(BlockLength = c(5L, 10L, 20L, 40L), HACLag = c(5L, 10L, 20L, 40L))
sensitivity <- rbindlist(lapply(seq_len(nrow(grid)), function(i) {
  rbind(one_sensitivity("PostAI", dat$PostAI, grid$BlockLength[i], grid$HACLag[i]),
        one_sensitivity("HighVIX", dat$HighVIX, grid$BlockLength[i], grid$HACLag[i]))
}))
fwrite(sensitivity, file.path(PATHS$tables, "reviewer2_block_hac_sensitivity.csv"))

# A covariance-only GFEVD benchmark sets all lag propagation matrices to zero;
# it therefore retains contemporaneous covariance while removing VAR dynamics.
cov_theta <- function(S) {
  K <- nrow(S); th <- matrix(0, K, K, dimnames = dimnames(S))
  for (i in seq_len(K)) for (j in seq_len(K)) th[i, j] <- S[i, j]^2 / (S[i, i] * S[j, j])
  th / rowSums(th)
}
cov_tci <- vapply(seq_len(dim(cobj$theta)[1]), function(i) {
  # Recover the same date-specific residual covariance through a 1-step, zero-dynamics proxy:
  # use the covariance implied by the full theta denominator is not identified, so estimate on a rolling window.
  z <- ret[Date <= cobj$dynamic$Date[i], ..nodes]
  S <- cov(tail(as.matrix(z), PROJECT$init_window))
  th <- cov_theta(S); off <- th; diag(off) <- 0; 100 * sum(off) / nrow(th)
}, numeric(1))
full_tci <- cobj$dynamic$TCI
cov_compare <- data.table(Measure = c("Average TCI", "Correlation with dynamic TCI", "Post-AI difference", "High-VIX difference"),
  Dynamic = c(mean(full_tci), 1, contrast(full_tci, dat$PostAI), contrast(full_tci, dat$HighVIX)),
  CovarianceOnly = c(mean(cov_tci), cor(full_tci, cov_tci), contrast(cov_tci, dat$PostAI), contrast(cov_tci, dat$HighVIX)))
fwrite(cov_compare, file.path(PATHS$tables, "reviewer2_covariance_only_benchmark.csv"))

# Exclude the documented index-transition intervals, then re-estimate the same TVP-VAR.
segments <- data.table(Sample = c("Pre-URA-reconstitution", "Stable-between-reconstitutions", "Post-ICLN-reconstitution"),
  Start = as.Date(c("2010-11-09", "2018-08-01", "2021-04-19")), End = as.Date(c("2018-04-30", "2021-04-16", "2025-12-31")))
subsample <- rbindlist(lapply(seq_len(nrow(segments)), function(i) {
  x <- ret[Date >= segments$Start[i] & Date <= segments$End[i]]
  fit <- run_discount_tvp(as.matrix(x[, ..nodes]), x$Date, init_window = min(PROJECT$init_window, floor(nrow(x) * .25)))
  data.table(Sample = segments$Sample[i], Start = segments$Start[i], End = segments$End[i],
             N = nrow(x), MeanTCI = mean(fit$TCI), NET_NG = mean(fit$NET[, "NG"]),
             NET_HG = mean(fit$NET[, "HG"]), NET_URA = mean(fit$NET[, "URA"]),
             NET_ICLN = mean(fit$NET[, "ICLN"]), NET_SOX = mean(fit$NET[, "SOX"]))
}))
fwrite(subsample, file.path(PATHS$tables, "reviewer2_transition_exclusion_subsamples.csv"))
saveRDS(list(sensitivity = sensitivity, covariance_only = cov_compare, subsamples = subsample),
        file.path(PATHS$models, "09_reviewer2_connectedness_extensions.rds"))
