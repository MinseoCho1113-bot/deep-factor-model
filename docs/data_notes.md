# Data Notes: Proxies vs. True Fundamentals

This project uses free, price/volume-derived data as a substitute for the
CRSP/Compustat panel used in the original Gu-Kelly-Xiu (2021) paper. Document
limitations here for the writeup's methodology/limitations section.

| Characteristic in this project | True GKX equivalent | Proxy quality |
|---|---|---|
| `mkt_cap_proxy` (price × volume) | Market equity (price × shares outstanding) | Weak proxy — volume is not shares outstanding. Replace with a shares-outstanding source if available (e.g. a fundamentals API) before final results. |
| `mom_12_1` | 12-month momentum, skip most recent month | Faithful — computed directly from adjusted price. |
| `vol_60d` | Idiosyncratic/total return volatility | Faithful, though not residualized against a market model. |
| `amihud_illiq` | Amihud (2002) illiquidity | Faithful. |
| `dollar_vol_20d` | Turnover / dollar volume | Faithful. |

## Known Gaps

- No book-to-market, profitability, investment, or accrual characteristics
  (require balance-sheet/income-statement data — Compustat in the original
  paper). These are core characteristics in GKX; their absence limits how
  closely results can be compared to the paper's published R².
- `tq_index("SP500")` reflects the *current* S&P 500 membership, not
  historical constituents — introduces survivorship bias. Acceptable for a
  personal/portfolio project; flag explicitly as a limitation, and consider
  a historical constituents list (e.g. from a paid data vendor) as a
  stretch goal.
- Sample is a random `UNIVERSE_SIZE`-ticker subsample of S&P 500, not the
  full universe, purely for compute tractability on a personal machine.

## If Extending with Real Fundamentals

Options to fill the gap without WRDS access:
- SEC EDGAR structured data (`XBRL` via `edgar` or `finreportr` packages) —
  free but requires nontrivial parsing.
- A paid fundamentals API (e.g. via a `tidyquant` connector) if budget allows.
