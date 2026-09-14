#!/usr/bin/env Rscript
# 04_fit_ff5_benchmark.R
# Observable factor benchmark: Fama-French 5 + Momentum, via Ken French's
# data library. Regress each asset's excess return on the factors
# (rolling or full-sample OLS) as the linear "observable factor" baseline.

library(tidyverse)
library(frenchdata)
source("R/eval_metrics.R")

panel <- read_rds("data/processed/panel.rds")

ff5 <- download_french_data("Fama/French 5 Factors (2x3)")$subsets$data[[1]] %>%
  rename(Mkt.RF = `Mkt-RF`) %>%
  mutate(month = lubridate::ymd(paste0(date, "01"))) %>%
  select(month, Mkt.RF, SMB, HML, RMW, CMA, RF)

mom <- download_french_data("Momentum Factor (Mom)")$subsets$data[[1]] %>%
  mutate(month = lubridate::ymd(paste0(date, "01"))) %>%
  select(month, Mom)

factors_ff <- ff5 %>%
  inner_join(mom, by = "month") %>%
  mutate(across(c(Mkt.RF, SMB, HML, RMW, CMA, RF, Mom), ~ .x / 100))

merged <- panel %>%
  inner_join(factors_ff, by = "month") %>%
  mutate(excess_ret = ret - RF)

# Per-asset time-series OLS of excess return on the 6 factors.
fit_one_asset <- function(df) {
  lm(excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA + Mom, data = df)
}

ff5_fits <- merged %>%
  group_by(symbol) %>%
  group_modify(~ broom::tidy(fit_one_asset(.x))) %>%
  ungroup()

fitted_vals <- merged %>%
  group_by(symbol) %>%
  group_modify(~ mutate(.x, fitted = fitted(fit_one_asset(.x)))) %>%
  ungroup()

r2_in_sample <- total_r2(fitted_vals$excess_ret, fitted_vals$fitted)

dir.create("models", showWarnings = FALSE, recursive = TRUE)
write_rds(list(coefficients = ff5_fits, fitted = fitted_vals, factors = factors_ff),
          "models/ff5_benchmark.rds")

message(sprintf("FF5+Mom benchmark: in-sample total R^2 = %.4f", r2_in_sample))
