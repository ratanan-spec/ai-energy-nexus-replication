# GitHub release template

## Repository

- **Suggested name:** `ai-energy-nexus-replication`
- **Visibility:** Public after the verified-release checklist is complete.
- **Description:** Reproducible R code for TVP-VAR connectedness, stress regimes, and leakage-safe nonlinear forecasting in the AI-energy nexus.
- **Topics:** `econometrics`, `tvp-var`, `connectedness`, `energy-markets`, `machine-learning`, `shap`, `r`, `reproducible-research`

## First release

**Tag:** `v1.0.0-rc1`

**Title:** AI-Energy Nexus Replication Code v1.0.0-rc1

**Body:**

This release provides the data-free R implementation accompanying Bunnag (2026), *Energies*, 19, 4979. It includes the TVP-VAR/GFEVD workflow, dependence-aware regime inference, leakage-safe forecasting, HAC-DM, Model Confidence Set, grouped interventional Shapley attribution, and robustness extensions.

No vendor data, processed returns, row-level forecasts, or archived outputs are included. Users must supply licensed data and complete the documented provenance and verification steps. This is a release candidate; see `TESTING_STATUS.md`, `VALIDATION_EVIDENCE.md`, and `RELEASE_CHECKLIST.md`.

## Verified release

After a clean R 4.3+ rerun passes `RELEASE_CHECKLIST.md`, replace the release-candidate label with `v1.0.0`, update `TESTING_STATUS.md`, and record the final R/session information.
