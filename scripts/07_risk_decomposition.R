#!/usr/bin/env Rscript
# 07_risk_decomposition.R
# Risk analytics payoff: use the fitted factor models to decompose
# portfolio risk (component VaR / marginal contribution to risk) and
# compare linear (PCA/FF5) vs. nonlinear (autoencoder) factor structures.

library(tidyverse)
library(PerformanceAnalytics)
source("R/eval_metrics.R")

pca <- read_rds("models/pca_benchmark.rds")
ae_meta <- read_rds("models/autoencoder_meta.rds")

# Equal-weighted test portfolio over the PCA symbol universe.
weights_pca <- rep(1 / ncol(pca$ret_mat), ncol(pca$ret_mat))
port_ret_pca <- as.numeric(pca$ret_mat %*% weights_pca)

# --- Linear (PCA) factor risk decomposition ---
# Component VaR via the delta-normal method using factor covariance.
factor_cov_pca <- cov(pca$factors)
loadings_pca <- pca$loadings

port_loading_pca <- t(weights_pca) %*% pca$loadings  # portfolio's factor exposure? note: weights over assets, loadings are per-asset x K
# NOTE: loadings_pca is [n_assets, K]; weights_pca must align with the same
# asset ordering as pca$symbols for this matrix multiply to be meaningful.
port_factor_exposure <- t(loadings_pca) %*% weights_pca  # [K, 1]

port_var_pca <- t(port_factor_exposure) %*% factor_cov_pca %*% port_factor_exposure
message(sprintf("PCA factor-implied portfolio variance (monthly): %.6f", port_var_pca))

# --- Historical VaR/CVaR on realized portfolio returns (linear benchmark) ---
var_hist <- VaR(port_ret_pca, p = 0.95, method = "historical")
cvar_hist <- ES(port_ret_pca, p = 0.95, method = "historical")

message(sprintf("Historical VaR(95%%): %.4f | CVaR(95%%): %.4f", var_hist, cvar_hist))

# --- Autoencoder-implied portfolio risk ---
weights_ae <- rep(1 / ncol(ae_meta$ret_mat), ncol(ae_meta$ret_mat))
port_ret_ae <- as.numeric(ae_meta$ret_mat %*% weights_ae)
var_hist_ae <- VaR(port_ret_ae, p = 0.95, method = "historical")
cvar_hist_ae <- ES(port_ret_ae, p = 0.95, method = "historical")

message(sprintf("[Autoencoder universe] Historical VaR(95%%): %.4f | CVaR(95%%): %.4f",
                 var_hist_ae, cvar_hist_ae))

risk_comparison <- tibble(
  model = c("PCA-universe (historical)", "Autoencoder-universe (historical)"),
  VaR_95 = c(var_hist, var_hist_ae),
  CVaR_95 = c(cvar_hist, cvar_hist_ae)
)

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
write_csv(risk_comparison, "output/tables/risk_comparison.csv")
print(risk_comparison)

message("NEXT (stretch goal): component VaR decomposition using the ",
        "autoencoder's time-varying betas (beta_net output per period), ",
        "compared against the PCA's static loadings — this is where the ",
        "nonlinear model's marginal risk contribution should diverge most ",
        "from the linear benchmark during regime shifts.")
