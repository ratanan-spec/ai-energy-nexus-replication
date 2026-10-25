# AI-Energy Nexus: Public Code Release Candidate v1.0

Code companion for Tanattrin Bunnag (2026), “Stress-Contingent Risk Transmission in the AI-Energy Nexus: Evidence from TVP-VAR Connectedness and Nonlinear Predictive Models”, *Energies*, 19(20), 4979. https://doi.org/10.3390/en19204979

## Purpose and status

This repository contains the R implementation of the published empirical workflow: discount-factor TVP-VAR connectedness, generalized FEVD, stress and calendar comparisons, stationary bootstrap, HAC regression, timing and frequency checks, expanding-window machine-learning forecasts, HAC Diebold-Mariano tests, Model Confidence Set calculation, and grouped interventional Shapley attribution.

**Release candidate, pending an environment-specific full rerun.** The code has been audited structurally and existing archived outputs were inspected, but a full execution was not possible in the current build environment because R is unavailable. Do not describe a rerun as verified until the commands below complete with an audited input file and the results are compared with `EXPECTED_RESULTS.md`.

`VALIDATION_EVIDENCE.md` records the archived input/output checks that agree with the published record; it also distinguishes those checks from an independent full rerun.

## Data and licence

No raw, normalized, processed, or row-level forecast data are included. The published study used daily prices from Investing.com. Users must obtain data under the provider's terms; the code does not grant redistribution rights. Before running, create `data/raw/vendor_prices_normalized.csv` with columns `Date,NG,HG,URA,ICLN,SOX,VIX`, one row per calendar date. See `DATA_PROVENANCE.md`.

`tools/normalize_vendor_dates.py` is optional and can be used privately when the original vendor workbook is available. It is not needed when a correctly normalized CSV has already been prepared.

## Reproduce

Use R 4.3 or later from the project root:

```r
source("install_packages.R") # first use only
source("preflight.R")
source("00_run_all.R")
source("verify_published_results.R")
```

The scripts use seed 2026 and write generated products under `outputs/`. The code enforces exact-date intersection and does not forward fill or interpolate prices.

Use `RELEASE_CHECKLIST.md` before changing the release-candidate label to a verified package.

## Publication checks

The published sample has 3,810 common prices, 3,809 returns, and 3,558 dynamic-connectedness observations. The principal reported results are an average TCI of 30.55%, a high-VIX minus normal-VIX contrast of 10.01 percentage points, a joint HAC high-VIX coefficient of 9.84 points, and a post-30-November-2022 HAC coefficient of -2.08 points. Differences require an explanation of data vendor identifiers, roll and adjustment rules, close/settlement convention, corporate actions, package version, and date-normalization choices.

## Interpretation

VIX is an external stress and forecasting control, not a sixth connectedness node. The post-November-2022 comparison is a prespecified calendar contrast, not a causal treatment. GFEVD connectedness, forecast accuracy, and Shapley attribution are distinct quantities; grouped Shapley shares do not establish structural spillovers or causality.

## Citation

Please cite the *Energies* article above when using this implementation.
