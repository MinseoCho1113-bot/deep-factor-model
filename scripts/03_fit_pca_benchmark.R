#!/usr/bin/env Rscript
# 03_fit_pca_benchmark.R
# Linear statistical factor benchmark: PCA on the (balanced) panel of
# monthly returns. This is the "unconditional linear factor" baseline
# against which the autoencoder is compared.

library(tidyverse)
source("R/eval_metrics.R")

panel <- read_rds("data/processed/panel.rds")

K_FACTORS <- 5  # match dimensionality of FF5 for a fair comparison

wide_ret <- panel %>%
  select(symbol, month, ret) %>%
  pivot_wider(names_from = symbol, values_from = ret) %>%
  arrange(month)

ret_mat <- wide_ret %>% select(-month) %>% as.matrix()
# Balanced panel required for plain PCA; drop columns with any NA for now.
keep_cols <- colSums(is.na(ret_mat)) == 0
ret_mat <- ret_mat[, keep_cols]

pca_fit <- prcomp(ret_mat, center = TRUE, scale. = FALSE)
factors_pca <- pca_fit$x[, 1:K_FACTORS]
loadings_pca <- pca_fit$rotation[, 1:K_FACTORS]

dir.create("models", showWarnings = FALSE, recursive = TRUE)
write_rds(list(factors = factors_pca, loadings = loadings_pca, months = wide_ret$month,
                ret_mat = ret_mat, symbols = colnames(ret_mat)),
          "models/pca_benchmark.rds")

r2_in_sample <- total_r2(ret_mat, factors_pca %*% t(loadings_pca))
message(sprintf("PCA benchmark: K=%d factors, in-sample total R^2 = %.4f",
                 K_FACTORS, r2_in_sample))
