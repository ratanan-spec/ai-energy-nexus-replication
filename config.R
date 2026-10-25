PROJECT <- list(
  title = "AI-Energy Market Connectedness",
  author = "Tanattrin Bunnag",
  start_date = as.Date("2010-11-04"),
  end_date = as.Date("2025-12-31"),
  ai_cutoff = as.Date("2022-11-30"),
  nodes = c("NG", "HG", "URA", "ICLN", "SOX"),
  control = "VIX",
  return_scale = 100,
  var_lag = 1L,
  init_window = 251L,
  fevd_horizon = 10L,
  lambda = 0.99,
  kappa = 0.96,
  stability_bound = 0.995,
  vix_quantile = 0.75,
  bootstrap_B = 2000L,
  bootstrap_block = 10,
  hac_lag = 10L,
  forecast_lags = 5L,
  forecast_initial_fraction = 0.70,
  forecast_refit = 252L,
  tree_count = 200L,
  min_node_size = 5L,
  mcs_alpha = 0.10,
  mcs_B = 500L,
  seed = 2026L
)

PATHS <- list(
  raw = file.path("data", "raw", "vendor_prices_normalized.csv"),
  processed = file.path("data", "processed", "aligned_returns.csv"),
  outputs = "outputs",
  tables = file.path("outputs", "tables"),
  figures = file.path("outputs", "figures"),
  models = file.path("outputs", "models"),
  logs = "logs"
)

required_packages <- c(
  "readxl", "data.table", "ggplot2", "patchwork", "tseries", "urca",
  "sandwich", "lmtest", "glmnet", "ranger", "xgboost", "igraph"
)

ensure_directories <- function() {
  invisible(lapply(unname(PATHS[c("processed", "outputs", "tables", "figures", "models", "logs")]),
                   function(p) dir.create(if (grepl("\\.csv$", p)) dirname(p) else p,
                                          recursive = TRUE, showWarnings = FALSE)))
}

assert_packages <- function() {
  missing <- required_packages[!vapply(required_packages, requireNamespace,
                                       quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing)) {
    stop("Missing R packages: ", paste(missing, collapse = ", "),
         ". Install them before running 00_run_all.R.")
  }
}
