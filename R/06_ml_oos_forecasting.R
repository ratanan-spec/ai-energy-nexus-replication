library(data.table)
library(glmnet)
library(ranger)
library(xgboost)

ret <- fread(PATHS$processed)
ret[, Date := as.Date(Date)]
for (nm in PROJECT$nodes) for (ell in seq_len(PROJECT$forecast_lags))
  ret[, paste0(nm, "_L", ell) := shift(get(nm), ell)]
ret[, VIX_L1 := shift(VIX, 1)]
ret[, DVIX_L1 := shift(VIX_Delta, 1)]
ret <- na.omit(ret)

source_groups <- c(PROJECT$nodes, "VIX")
all_features <- c(unlist(lapply(PROJECT$nodes, function(nm) paste0(nm, "_L", 1:PROJECT$forecast_lags))),
                  "VIX_L1", "DVIX_L1")

fit_predict <- function(model, Xtr, ytr, Xte) {
  if (model == "Mean") return(rep(mean(ytr), nrow(Xte)))
  if (model == "Ridge") {
    cv <- cv.glmnet(Xtr, ytr, alpha = 0, nfolds = 5, type.measure = "mse")
    return(drop(predict(cv, Xte, s = "lambda.min")))
  }
  if (model %in% c("OwnExtraTrees", "FullExtraTrees", "FullRF")) {
    splitrule <- if (grepl("ExtraTrees", model)) "extratrees" else "variance"
    fit <- ranger(x = as.data.frame(Xtr), y = ytr, num.trees = PROJECT$tree_count,
                  min.node.size = PROJECT$min_node_size, splitrule = splitrule,
                  seed = PROJECT$seed, num.threads = 1)
    return(predict(fit, as.data.frame(Xte))$predictions)
  }
  if (model == "FullHGB") {
    fit <- xgboost(data = Xtr, label = ytr, nrounds = 200, max_depth = 3,
                   eta = .03, subsample = .8, colsample_bytree = .8,
                   tree_method = "hist", objective = "reg:squarederror",
                   verbose = 0, nthread = 1)
    return(predict(fit, Xte))
  }
  stop("Unknown model: ", model)
}

models <- c("Mean", "Ridge", "OwnExtraTrees", "FullExtraTrees", "FullRF", "FullHGB")
all_predictions <- list(); fitted_blocks <- list()
for (target in PROJECT$nodes) {
  own_features <- c(paste0(target, "_L", 1:PROJECT$forecast_lags), "VIX_L1", "DVIX_L1")
  n <- nrow(ret); first_test <- floor(PROJECT$forecast_initial_fraction * n) + 1L
  origins <- seq(first_test, n, by = PROJECT$forecast_refit)
  target_preds <- list()
  for (b in seq_along(origins)) {
    lo <- origins[b]; hi <- min(n, lo + PROJECT$forecast_refit - 1L)
    train <- seq_len(lo - 1L); test <- lo:hi
    if (max(train) >= min(test)) stop("Chronology violation in forecast split.")
    for (model in models) {
      feats <- if (model %in% c("Mean", "Ridge", "OwnExtraTrees")) own_features else all_features
      Xtr <- as.matrix(ret[train, ..feats]); Xte <- as.matrix(ret[test, ..feats]); ytr <- ret[[target]][train]
      pred <- fit_predict(model, Xtr, ytr, Xte)
      target_preds[[length(target_preds) + 1L]] <- data.table(
        Target = target, Model = model, Block = b, Date = ret$Date[test],
        Actual = ret[[target]][test], Forecast = pred)
    }
  }
  all_predictions[[target]] <- rbindlist(target_preds)
}
predictions <- rbindlist(all_predictions)
predictions[, `:=`(Error = Actual - Forecast, AE = abs(Actual - Forecast), SE = (Actual - Forecast)^2)]
accuracy <- predictions[, .(N = .N, RMSE = sqrt(mean(SE)), MAE = mean(AE)), by = .(Target, Model)]
fwrite(predictions, file.path(PATHS$tables, "oos_predictions.csv"))
fwrite(accuracy, file.path(PATHS$tables, "oos_accuracy.csv"))
saveRDS(list(data = ret, predictions = predictions, accuracy = accuracy,
             all_features = all_features), file.path(PATHS$models, "06_oos_forecasts.rds"))
