#!/usr/bin/env Rscript
# 01_download_data.R
# Pull a proxy equity universe (S&P 500 constituents) and daily/monthly
# prices via tidyquant. This is a free-data substitute for a CRSP/WRDS pull.

library(tidyverse)
library(tidyquant)

UNIVERSE_SIZE <- 200   # subsample for tractability; raise once pipeline is validated
START_DATE    <- "2000-01-01"
END_DATE      <- Sys.Date()

set.seed(42)
sp500 <- tq_index("SP500") %>%
  filter(!is.na(symbol), symbol != "") %>%
  distinct(symbol, company, sector) %>%
  slice_sample(n = UNIVERSE_SIZE)

message(sprintf("Downloading daily prices for %d tickers from %s to %s",
                 nrow(sp500), START_DATE, END_DATE))

prices <- sp500 %>%
  tq_get(get = "stock.prices", from = START_DATE, to = END_DATE) %>%
  drop_na(adjusted)

monthly_returns <- prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn,
               period = "monthly", col_rename = "ret") %>%
  ungroup()

dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
write_rds(sp500, "data/raw/universe.rds")
write_rds(prices, "data/raw/daily_prices.rds")
write_rds(monthly_returns, "data/raw/monthly_returns.rds")

message("Saved: data/raw/universe.rds, daily_prices.rds, monthly_returns.rds")
