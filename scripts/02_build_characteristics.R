#!/usr/bin/env Rscript
# 02_build_characteristics.R
# Construct firm-characteristic panel z_{i,t-1} from price/volume data.
# These are proxies for the Compustat-derived characteristics used in
# Gu-Kelly-Xiu (2021); see docs/data_notes.md for what is/isn't a true
# fundamental-based characteristic.

library(tidyverse)
library(tidyquant)
source("R/characteristics.R")

daily_prices <- read_rds("data/raw/daily_prices.rds")
monthly_returns <- read_rds("data/raw/monthly_returns.rds")

characteristics <- daily_prices %>%
  group_by(symbol) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    mkt_cap_proxy = adjusted * volume,          # crude size proxy (no shares outstanding)
    dollar_vol_20d = roll_mean(adjusted * volume, 20),
    mom_12_1 = mom_12_1(adjusted),               # 12-month return skipping most recent month
    vol_60d = roll_sd_return(adjusted, 60),
    amihud_illiq = amihud_illiquidity(adjusted, volume, 20)
  ) %>%
  ungroup()

# Collapse to month-end characteristics, lagged one month relative to the
# return they will be used to explain (avoid look-ahead bias).
monthly_chars <- characteristics %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(symbol, month) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(symbol, month, mkt_cap_proxy, dollar_vol_20d, mom_12_1, vol_60d, amihud_illiq) %>%
  group_by(symbol) %>%
  arrange(month, .by_group = TRUE) %>%
  mutate(across(c(mkt_cap_proxy, dollar_vol_20d, mom_12_1, vol_60d, amihud_illiq),
                ~ lag(.x), .names = "{.col}_lag1")) %>%
  ungroup()

panel <- monthly_returns %>%
  mutate(month = floor_date(date, "month")) %>%
  inner_join(monthly_chars, by = c("symbol", "month")) %>%
  drop_na()

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_rds(panel, "data/processed/panel.rds")
message(sprintf("Saved panel: %d obs, %d firms, %d months",
                 nrow(panel), n_distinct(panel$symbol), n_distinct(panel$month)))
