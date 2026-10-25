library(readxl)
library(data.table)

parse_vendor_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  z <- trimws(as.character(x))
  out <- suppressWarnings(as.Date(z, tryFormats = c("%m/%d/%Y", "%Y-%m-%d", "%d/%m/%Y")))
  numeric_like <- suppressWarnings(as.numeric(z))
  idx <- is.na(out) & !is.na(numeric_like)
  out[idx] <- as.Date(numeric_like[idx], origin = "1899-12-30")
  out
}

normalized_path <- file.path("data", "raw", "vendor_prices_normalized.csv")
if (!file.exists(normalized_path))
  stop("Missing vendor_prices_normalized.csv. See README for the mixed-date normalization rule.")
normalized <- fread(normalized_path)
normalized[, Date := as.Date(Date)]

# The vendor workbook stores independent date-price pairs in B:C, D:E, ..., L:M.
pairs <- list(NG = c(2L, 3L), HG = c(4L, 5L), URA = c(6L, 7L),
              ICLN = c(8L, 9L), SOX = c(10L, 11L), VIX = c(12L, 13L))

long_list <- lapply(names(pairs), function(nm) {
  d <- normalized[!is.na(get(nm)), .(Date, Price = as.numeric(get(nm)))]
  d <- d[!is.na(Date) & !is.na(Price) & Price > 0]
  d <- d[Date >= PROJECT$start_date & Date <= PROJECT$end_date]
  setorder(d, Date)
  d <- unique(d, by = "Date", fromLast = TRUE)
  setnames(d, "Price", nm)
  d
})
names(long_list) <- names(pairs)

audit <- rbindlist(lapply(names(long_list), function(nm) {
  d <- long_list[[nm]]
  data.table(Series = nm, RawValidRows = nrow(d), FirstDate = min(d$Date), LastDate = max(d$Date),
             DuplicateDatesAfterCleaning = anyDuplicated(d$Date))
}))
fwrite(audit, file.path(PATHS$tables, "data_source_audit.csv"))

prices <- Reduce(function(x, y) merge(x, y, by = "Date", all = FALSE), long_list)
setorder(prices, Date)
if (anyNA(prices)) stop("Exact-date intersection unexpectedly contains missing values.")
if (anyDuplicated(prices$Date)) stop("Duplicate dates remain after cleaning.")

ret <- copy(prices)
for (nm in PROJECT$nodes) ret[, (nm) := PROJECT$return_scale * c(NA_real_, diff(log(get(nm))))]
ret[, VIX_Delta := c(NA_real_, diff(VIX))]
ret <- ret[-1L]

if (nrow(prices) != 3810L || nrow(ret) != 3809L) {
  warning(sprintf("Expected 3810 common prices and 3809 returns; obtained %d and %d. Check vendor revisions.",
                  nrow(prices), nrow(ret)))
}
fwrite(prices, file.path("data", "processed", "aligned_prices.csv"))
fwrite(ret, PATHS$processed)
saveRDS(list(prices = prices, returns = ret, audit = audit),
        file.path(PATHS$models, "01_aligned_data.rds"))
