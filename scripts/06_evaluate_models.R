#!/usr/bin/env Rscript
# 06_evaluate_models.R
# Compare PCA, FF5+Mom, and autoencoder factor models on:
#   - in-sample total R^2
#   - Sharpe ratio of implied factor-mimicking portfolios
#   - average absolute pricing error (alpha), where applicable
#
# Note: universes differ slightly across the three benchmarks (PCA/AE use
# balanced panels with different dropout, FF5 is a per-asset OLS on the
# full panel) so treat this as a directional comparison, not an exact
# apples-to-apples test. For a rigorous writeup, restrict all three to the
# common balanced symbol set before running this script.

library(tidyverse)
source("R/eval_metrics.R")
source("R/autoencoder.R")

pca <- read_rds("models/pca_benchmark.rds")
ff5 <- read_rds("models/ff5_benchmark.rds")
ae_meta <- read_rds("models/autoencoder_meta.rds")
ae_model <- torch::torch_load("models/autoencoder_model.pt")

results <- tibble(
  model = c("PCA (K=5)", "FF5 + Momentum", "Conditional Autoencoder (K=5)"),
  in_sample_r2 = NA_real_,
  mimicking_sharpe = NA_real_
)

# --- PCA ---
pca_rhat <- pca$factors %*% t(pca$loadings)
results$in_sample_r2[1] <- total_r2(pca$ret_mat, pca_rhat)
pca_factor_ret <- rowMeans(pca$factors)  # crude factor-portfolio proxy
results$mimicking_sharpe[1] <- sharpe_ratio(pca_factor_ret)

# --- FF5 ---
results$in_sample_r2[2] <- total_r2(ff5$fitted$excess_ret, ff5$fitted$fitted)
results$mimicking_sharpe[2] <- sharpe_ratio(ff5$factors$Mkt.RF)

# --- Autoencoder ---
n_time <- dim(ae_meta$char_array)[1]
n_assets <- dim(ae_meta$char_array)[2]
ae_rhat <- matrix(NA_real_, nrow = n_time, ncol = n_assets)
ae_factors <- matrix(NA_real_, nrow = n_time, ncol = ae_meta$k)

for (t in seq_len(n_time)) {
  z_t <- ae_meta$char_array[t, , ]
  r_t <- ae_meta$ret_mat[t, ]
  bf <- extract_beta_factor(ae_model, z_t, r_t)
  ae_rhat[t, ] <- bf$beta %*% bf$factor
  ae_factors[t, ] <- bf$factor
}

results$in_sample_r2[3] <- total_r2(ae_meta$ret_mat, ae_rhat)
results$mimicking_sharpe[3] <- sharpe_ratio(rowMeans(ae_factors))

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
write_csv(results, "output/tables/model_comparison.csv")
print(results)

message("Saved: output/tables/model_comparison.csv")
message("NEXT: split into train/test windows for a genuine out-of-sample ",
        "predictive R^2 (this script currently reports in-sample fit only).")
