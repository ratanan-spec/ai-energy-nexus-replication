# Data Dictionary

| Field | Role | Unit or definition |
|---|---|---|
| Date | Trading date | ISO date |
| NG | Connectedness node | Henry Hub natural-gas futures closing price or percentage log return |
| HG | Connectedness node | Copper futures closing price or percentage log return |
| URA | Connectedness node | Global X Uranium ETF closing price or percentage log return |
| ICLN | Connectedness node | iShares Global Clean Energy ETF closing price or percentage log return |
| SOX | Connectedness node | PHLX Semiconductor Index closing level or percentage log return |
| VIX | External control | CBOE Volatility Index level |
| VIX_Delta | External control | One-day change in VIX level |
| PostAI | Regime indicator | 1 from 30 November 2022, otherwise 0 |
| HighVIX | Regime indicator | 1 when VIX is at or above its sample 75th percentile |
| TCI | Connectedness output | Percentage of normalized H-step forecast-error variance attributable to cross-market shocks |
| TO_i | Directional output | Shocks transmitted by node i to all other nodes |
| FROM_i | Directional output | Shocks received by node i from all other nodes |
| NET_i | Directional output | TO_i minus FROM_i |
