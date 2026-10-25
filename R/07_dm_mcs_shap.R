library(data.table)
library(ranger)

oos <- readRDS(file.path(PATHS$models, "06_oos_forecasts.rds"))
pred <- oos$predictions

stationary_indices <- function(n, mean_block) {
  p <- 1 / mean_block; idx <- integer(n); idx[1] <- sample.int(n, 1)
  for (i in 2:n) idx[i] <- if (runif(1) < p) sample.int(n, 1) else (idx[i-1] %% n) + 1L
  idx
}

subsets_of <- function(x) {
  unlist(lapply(0:length(x), function(k) {
    if (k == 0L) list(character()) else combn(x, k, simplify = FALSE)
  }), recursive = FALSE)
}

nw_variance_mean <- function(x, lag = NULL) {
  x <- x - mean(x); n <- length(x)
  if (is.null(lag)) lag <- floor(4 * (n / 100)^(2/9))
  g0 <- sum(x*x) / n; lrv <- g0
  if (lag > 0) for (k in 1:lag) {
    gk <- sum(x[(k+1):n] * x[1:(n-k)]) / n
    lrv <- lrv + 2 * (1 - k/(lag+1)) * gk
  }
  lrv / n
}

dm_one <- function(target, model, benchmark = "OwnExtraTrees") {
  a <- pred[Target == target & Model == model, .(Date, loss_m = SE)]
  b <- pred[Target == target & Model == benchmark, .(Date, loss_b = SE)]
  z <- merge(a, b, by = "Date"); d <- z$loss_m - z$loss_b
  se <- sqrt(nw_variance_mean(d)); stat <- mean(d) / se
  data.table(Target = target, Model = model, Benchmark = benchmark, N = length(d),
             MeanLossDifference = mean(d), DM_HAC = stat,
             p = 2 * pt(abs(stat), df = length(d) - 1, lower.tail = FALSE))
}
dm <- rbindlist(lapply(PROJECT$nodes, function(tg)
  rbindlist(lapply(setdiff(unique(pred$Model), "OwnExtraTrees"), function(m) dm_one(tg, m)))))
fwrite(dm, file.path(PATHS$tables, "dm_hac_tests.csv"))

# Dependence-aware MCS-style sequential elimination using the range statistic.
mcs_target <- function(target, alpha = PROJECT$mcs_alpha, B = PROJECT$mcs_B) {
  wide <- dcast(pred[Target == target], Date ~ Model, value.var = "SE")
  L <- as.matrix(wide[, -1]); active <- colnames(L); elimination <- list(); step <- 0L
  while (length(active) > 1L) {
    step <- step + 1L; A <- L[, active, drop = FALSE]
    means <- colMeans(A); stat <- max(means) - min(means)
    set.seed(PROJECT$seed + step)
    boot <- replicate(B, {
      ii <- stationary_indices(nrow(A), PROJECT$bootstrap_block)
      centered <- A - matrix(colMeans(A), nrow(A), ncol(A), byrow = TRUE)
      mb <- colMeans(centered[ii, , drop = FALSE])
      max(mb) - min(mb)
    })
    p <- mean(boot >= stat)
    if (p > alpha) break
    worst <- names(which.max(means))
    elimination[[length(elimination)+1L]] <- data.table(Target = target, Step = step,
      Eliminated = worst, Statistic = stat, p = p)
    active <- setdiff(active, worst)
  }
  list(retained = data.table(Target = target, Model = active, Retained90 = TRUE),
       path = rbindlist(elimination, fill = TRUE))
}
mcs_list <- lapply(PROJECT$nodes, mcs_target)
mcs_retained <- rbindlist(lapply(mcs_list, `[[`, "retained"))
mcs_path <- rbindlist(lapply(mcs_list, `[[`, "path"), fill = TRUE)
fwrite(mcs_retained, file.path(PATHS$tables, "mcs_90_retained.csv"))
fwrite(mcs_path, file.path(PATHS$tables, "mcs_elimination_path.csv"))

# Exact group-level interventional Shapley values for six source blocks.
group_features <- c(setNames(lapply(PROJECT$nodes, function(nm) paste0(nm, "_L", 1:PROJECT$forecast_lags)),
                             PROJECT$nodes),
                    list(VIX = c("VIX_L1", "DVIX_L1")))
all_groups <- names(group_features); G <- length(all_groups)
coalitions <- subsets_of(all_groups)

coalition_prediction <- function(fit, X, baseline, coalition) {
  Xc <- X
  excluded <- setdiff(all_groups, coalition)
  for (g in excluded) Xc[, group_features[[g]]] <- baseline[group_features[[g]]]
  predict(fit, as.data.frame(Xc))$predictions
}

shapley_block <- function(target, block_id) {
  dat <- oos$data; n <- nrow(dat); first_test <- floor(PROJECT$forecast_initial_fraction*n)+1L
  lo <- first_test + (block_id - 1L) * PROJECT$forecast_refit
  hi <- min(n, lo + PROJECT$forecast_refit - 1L)
  train <- seq_len(lo - 1L); test <- lo:hi
  feats <- oos$all_features
  fit <- ranger(x = as.data.frame(dat[train, ..feats]), y = dat[[target]][train],
                num.trees = PROJECT$tree_count, min.node.size = PROJECT$min_node_size,
                splitrule = "extratrees", seed = PROJECT$seed, num.threads = 1)
  X <- as.data.frame(dat[test, ..feats]); baseline <- vapply(dat[train, ..feats], mean, numeric(1))
  cache <- new.env(parent = emptyenv())
  key <- function(S) paste0("S:", paste(sort(S), collapse = "|"))
  get_pred <- function(S) {
    k <- key(S); if (!exists(k, cache, inherits = FALSE))
      assign(k, coalition_prediction(fit, X, baseline, S), cache)
    get(k, cache, inherits = FALSE)
  }
  ans <- rbindlist(lapply(all_groups, function(g) {
    others <- setdiff(all_groups, g); phi <- rep(0, nrow(X))
    for (S in subsets_of(others)) {
      w <- factorial(length(S)) * factorial(G-length(S)-1) / factorial(G)
      phi <- phi + w * (get_pred(c(S, g)) - get_pred(S))
    }
    data.table(Target = target, Block = block_id, Date = dat$Date[test], Group = g,
               AbsPhi = abs(phi))
  }))
  ans
}

nblocks <- ceiling((nrow(oos$data) - (floor(PROJECT$forecast_initial_fraction*nrow(oos$data))+1L) + 1L) /
                   PROJECT$forecast_refit)
shap_long <- rbindlist(lapply(PROJECT$nodes, function(tg)
  rbindlist(lapply(seq_len(nblocks), function(b) shapley_block(tg, b)))))
shap_summary <- shap_long[, .(MeanAbsPhi = mean(AbsPhi)), by = .(Target, Group)]
shap_summary[, Share := 100 * MeanAbsPhi / sum(MeanAbsPhi), by = Target]
fwrite(shap_long, file.path(PATHS$tables, "grouped_shap_oos_long.csv"))
fwrite(shap_summary, file.path(PATHS$tables, "grouped_shap_shares.csv"))
saveRDS(list(dm = dm, mcs_retained = mcs_retained, mcs_path = mcs_path,
             shap_long = shap_long, shap_summary = shap_summary),
        file.path(PATHS$models, "07_dm_mcs_shap.rds"))
