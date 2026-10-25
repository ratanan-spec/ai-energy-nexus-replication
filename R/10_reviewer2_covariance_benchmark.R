library(data.table)
cobj <- readRDS(file.path(PATHS$models, "03_tvp_connectedness.rds"))
ret <- fread(PATHS$processed)
nodes <- PROJECT$nodes
Y <- as.matrix(ret[, ..nodes])
cov_theta <- function(S) {
  K <- nrow(S); th <- matrix(0, K, K)
  for (i in seq_len(K)) for (j in seq_len(K)) th[i, j] <- S[i, j]^2 / (S[i, i] * S[j, j])
  th / rowSums(th)
}
Tn <- nrow(cobj$dynamic)
ends <- PROJECT$init_window + seq_len(Tn)
cov_tci <- vapply(ends, function(e) {
  S <- cov(Y[(e - PROJECT$init_window + 1L):e, , drop = FALSE])
  th <- cov_theta(S); diag(th) <- 0; 100 * sum(th) / nrow(th)
}, numeric(1))
dat <- copy(cobj$dynamic)
dat[, VIX := ret$VIX[ends]]
dat[, PostAI := as.integer(Date >= PROJECT$ai_cutoff)]
dat[, HighVIX := as.integer(VIX >= quantile(VIX, PROJECT$vix_quantile, na.rm = TRUE))]
contrast <- function(x, g) mean(x[g == 1]) - mean(x[g == 0])
cov_compare <- data.table(Measure = c("Average TCI", "Correlation with dynamic TCI", "Post-AI difference", "High-VIX difference"),
  Dynamic = c(mean(dat$TCI), 1, contrast(dat$TCI, dat$PostAI), contrast(dat$TCI, dat$HighVIX)),
  CovarianceOnly = c(mean(cov_tci), cor(dat$TCI, cov_tci), contrast(cov_tci, dat$PostAI), contrast(cov_tci, dat$HighVIX)))
fwrite(cov_compare, file.path(PATHS$tables, "reviewer2_covariance_only_benchmark.csv"))
saveRDS(list(covariance_only = cov_compare, cov_tci = cov_tci), file.path(PATHS$models, "10_reviewer2_covariance_benchmark.rds"))
