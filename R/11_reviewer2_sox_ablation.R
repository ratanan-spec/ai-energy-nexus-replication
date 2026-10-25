library(data.table)
library(ranger)

oos <- readRDS(file.path(PATHS$models, "06_oos_forecasts.rds"))
dat <- oos$data
all_features <- oos$all_features
fit_et <- function(Xtr, ytr, Xte) {
  fit <- ranger(x = as.data.frame(Xtr), y = ytr, num.trees = PROJECT$tree_count,
                min.node.size = PROJECT$min_node_size, splitrule = "extratrees",
                seed = PROJECT$seed, num.threads = 1)
  predict(fit, as.data.frame(Xte))$predictions
}
out <- list()
for (target in PROJECT$nodes) {
  own <- c(paste0(target, "_L", 1:PROJECT$forecast_lags), "VIX_L1", "DVIX_L1")
  sox <- paste0("SOX_L", 1:PROJECT$forecast_lags)
  sets <- list(Own = own, OwnPlusSOX = unique(c(own, sox)),
               FullWithoutSOX = setdiff(all_features, sox), FullWithSOX = all_features)
  n <- nrow(dat); origins <- seq(floor(PROJECT$forecast_initial_fraction * n) + 1L, n, by = PROJECT$forecast_refit)
  for (b in seq_along(origins)) {
    lo <- origins[b]; hi <- min(n, lo + PROJECT$forecast_refit - 1L); tr <- seq_len(lo - 1L); te <- lo:hi
    for (nm in names(sets)) {
      f <- sets[[nm]]; pr <- fit_et(as.matrix(dat[tr, ..f]), dat[[target]][tr], as.matrix(dat[te, ..f]))
      out[[length(out) + 1L]] <- data.table(Target = target, Model = nm, Block = b, Date = dat$Date[te],
        Actual = dat[[target]][te], Forecast = pr)
    }
  }
}
pred <- rbindlist(out); pred[, SE := (Actual - Forecast)^2]
accuracy <- pred[, .(N = .N, RMSE = sqrt(mean(SE)), MAE = mean(abs(Actual - Forecast))), by = .(Target, Model)]
nw_var_mean <- function(x, lag = PROJECT$hac_lag) {
  n <- length(x); z <- x - mean(x); g <- vapply(0:lag, function(k) sum(z[(k+1):n] * z[1:(n-k)]) / n, numeric(1))
  (g[1] + 2 * sum((1 - (1:lag)/(lag+1)) * g[-1])) / n
}
dm <- rbindlist(lapply(PROJECT$nodes, function(tg) rbindlist(lapply(c("OwnPlusSOX", "FullWithoutSOX", "FullWithSOX"), function(m) {
  a <- pred[Target == tg & Model == m, .(Date, loss = SE)]; b <- pred[Target == tg & Model == "Own", .(Date, bench = SE)]
  d <- merge(a, b, by = "Date")$loss - merge(a, b, by = "Date")$bench
  stat <- mean(d) / sqrt(nw_var_mean(d)); data.table(Target = tg, Model = m, Benchmark = "Own", N = length(d),
    MeanLossDifference = mean(d), DM_HAC = stat, p = 2 * pt(abs(stat), df = length(d)-1, lower.tail = FALSE))
}))))
fwrite(pred, file.path(PATHS$tables, "reviewer2_sox_ablation_predictions.csv"))
fwrite(accuracy, file.path(PATHS$tables, "reviewer2_sox_ablation_accuracy.csv"))
fwrite(dm, file.path(PATHS$tables, "reviewer2_sox_ablation_dm.csv"))
saveRDS(list(predictions = pred, accuracy = accuracy, dm = dm), file.path(PATHS$models, "11_reviewer2_sox_ablation.rds"))
