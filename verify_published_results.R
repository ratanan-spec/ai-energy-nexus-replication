# Run after 00_run_all.R. Stops if essential output files are absent and
# reports comparison values against the published article. This script does
# not convert a data-convention difference into a failure without explanation.
needed <- c(
  "outputs/tables/dynamic_connectedness.csv",
  "outputs/tables/regime_tests_bootstrap.csv",
  "outputs/tables/regime_hac_regression.csv",
  "outputs/tables/oos_accuracy.csv",
  "outputs/tables/reproducibility_checks.csv"
)
absent <- needed[!file.exists(needed)]
if (length(absent)) stop("Run 00_run_all.R first. Missing: ", paste(absent, collapse = ", "))

dynamic <- read.csv("outputs/tables/dynamic_connectedness.csv", check.names = FALSE)
checks <- read.csv("outputs/tables/reproducibility_checks.csv", check.names = FALSE)
published <- list(common_prices = 3810L, returns = 3809L, connectedness = 3558L,
                  mean_tci = 30.55, high_vix_difference = 10.01,
                  hac_high_vix = 9.84, hac_post_cutoff = -2.08, oos = 1142L)

value_for <- function(label) {
  hit <- checks[checks$Check == label, "Value"]
  if (!length(hit)) NA_real_ else as.numeric(hit[1])
}
report <- data.frame(
  Check = c("Common price dates", "Return observations", "Connectedness observations", "Mean TCI"),
  Published = c(published$common_prices, published$returns, published$connectedness, published$mean_tci),
  Generated = c(value_for("Common price dates"), value_for("Return observations"),
                value_for("Connectedness observations"), mean(dynamic$TCI, na.rm = TRUE))
)
report$Difference <- report$Generated - report$Published
write.csv(report, "outputs/tables/published_result_comparison.csv", row.names = FALSE)

if (!all(c("Value", "Expected") %in% names(checks))) warning("Unexpected reproducibility-check schema.")
cat("Published-result comparison written to outputs/tables/published_result_comparison.csv\n")
print(report, row.names = FALSE)
cat("\nReview regime, HAC, and OOS tables against EXPECTED_RESULTS.md before changing release status to verified.\n")
