# Verified-release checklist

Complete all items before renaming this project as an official verified replication package.

- [ ] `DATA_PROVENANCE.md` is completed privately for the input series.
- [ ] `source("preflight.R")` passes.
- [ ] `source("00_run_all.R")` finishes without error under R 4.3+.
- [ ] `source("verify_published_results.R")` produces its comparison table.
- [ ] Counts, dates, FEVD row sums, NET sums, and forecast chronology are checked.
- [ ] Regime, HAC, MCS, and grouped-Shapley outputs are reviewed against the published tables.
- [ ] Any difference is explained by documented vendor/data conventions or corrected code.
- [ ] The public repository contains no provider-restricted raw or row-level derived data.
- [ ] A release tag and the final software environment are recorded.
