# Testing status

## Completed

- Inspected the complete archived R workflow and its staged scripts.
- Cross-checked the private normalized input and archived outputs: sample counts, date span, regime estimates, HAC estimates, FEVD normalization, grouped-Shapley sums, forecast counts, and forecast chronology agree with the published record after rounding.
- Confirmed that the public archive excludes provider-supplied raw prices, normalized prices, processed returns, row-level forecasts, prior output tables, figures, and model objects.
- Checked that the public documentation uses the final published validation targets rather than pre-publication values in the earlier archived README.
- Ran Python syntax compilation successfully for `tools/normalize_vendor_dates.py`.

## Pending before a verified release

- Run `source("00_run_all.R")` under R 4.3+ after package installation and after supplying an audited local input file.
- Compare the regenerated values with `EXPECTED_RESULTS.md` and record any data-convention differences.
- Confirm that every output is reproducible from the permitted local input and that no provider-restricted data are committed.

The full R run is pending because the present build environment does not provide R or Rscript.
