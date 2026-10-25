# Archived validation evidence

This record documents checks performed on the author-retained analytical archive used to prepare this public, data-free code release. It does not substitute for an independent rerun.

## Input audit

The private normalized input had the required columns `Date, NG, HG, URA, ICLN, SOX, VIX`. Exact-date intersection over the article window produced 3,810 common prices from 8 November 2010 through 31 December 2025, hence 3,809 daily returns. URA began on 8 November 2010; all series had zero duplicate dates after cleaning.

## Archived output checks

The retained output table reported 3,810 price dates, 3,809 returns, and 3,558 connectedness observations. The automated checks reported valid TCI bounds, normalized GFEVD row-sum error of 1.11e-16, grouped-Shapley sum error of 0, and chronological out-of-sample ordering.

The retained regime output gave a high-VIX minus normal-VIX TCI difference of 10.0072 percentage points (normal 28.0435; high 38.0507). The joint HAC estimates were +9.8424 for HighVIX and -2.0758 for the post-30-November-2022 indicator. The retained forecast-accuracy table contained 1,142 common out-of-sample forecasts for every target-model combination.

These values agree with the published values after rounding. The public archive deliberately omits the input data and archived output products because their redistribution status has not been established.

## Remaining verification step

Run the public code with a licensed, audited local input under R 4.3+ and execute `verify_published_results.R`. Only then should the release-candidate status be changed to a verified replication release.
