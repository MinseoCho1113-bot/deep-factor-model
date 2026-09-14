#!/usr/bin/env Rscript
# 05_fit_autoencoder.R
# Fit the conditional autoencoder factor model and save results for
# comparison against PCA and FF5 benchmarks.

library(tidyverse)
source("R/autoencoder.R")
source("R/eval_metrics.R")

panel <- read_rds("data/processed/panel.rds")

CHAR_COLS <- c("mkt_cap_proxy_lag1", "dollar_vol_20d_lag1", "mom_12_1_lag1",
                "vol_60d_lag1", "amihud_illiq_lag1")
K_FACTORS <- 5

# Build a balanced panel: assets present in every month, in the same order.
balanced_symbols <- panel %>%
  count(symbol) %>%
  filter(n == max(n)) %>%
  pull(symbol)

bal <- panel %>%
  filter(symbol %in% balanced_symbols) %>%
  arrange(month, symbol)

months <- sort(unique(bal$month))
n_time <- length(months)
n_assets <- length(balanced_symbols)
n_chars <- length(CHAR_COLS)

char_array <- array(NA_real_, dim = c(n_time, n_assets, n_chars))
ret_mat <- matrix(NA_real_, nrow = n_time, ncol = n_assets)

for (t in seq_along(months)) {
  slice <- bal %>% filter(month == months[t]) %>% arrange(match(symbol, balanced_symbols))
  char_array[t, , ] <- as.matrix(slice[CHAR_COLS])
  ret_mat[t, ] <- slice$ret
}

# Standardize characteristics cross-sectionally at each t (rank/z-score),
# per GKX convention, to keep the beta network scale-invariant.
for (t in seq_along(months)) {
  for (c in seq_len(n_chars)) {
    x <- char_array[t, , c]
    char_array[t, , c] <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  }
}

message(sprintf("Training autoencoder: %d months, %d assets, %d characteristics, K=%d factors",
                 n_time, n_assets, n_chars, K_FACTORS))

fit <- train_autoencoder(char_array, ret_mat, k_factors = K_FACTORS,
                          epochs = 200, lr = 1e-3)

dir.create("models", showWarnings = FALSE, recursive = TRUE)
torch::torch_save(fit$model, "models/autoencoder_model.pt")
write_rds(list(loss_history = fit$loss_history, months = months,
                symbols = balanced_symbols, char_cols = CHAR_COLS, k = K_FACTORS,
                char_array = char_array, ret_mat = ret_mat),
          "models/autoencoder_meta.rds")

message("Saved: models/autoencoder_model.pt, models/autoencoder_meta.rds")
