# R/eval_metrics.R
# Shared evaluation metrics for comparing factor models (PCA, FF5, autoencoder).

#' Total R^2 (Gu-Kelly-Xiu definition): 1 - sum((r - rhat)^2) / sum(r^2)
#' Note this is NOT centered on the mean (consistent with the asset-pricing
#' ML literature, which evaluates on raw returns, not demeaned returns).
total_r2 <- function(r, rhat) {
  1 - sum((r - rhat)^2, na.rm = TRUE) / sum(r^2, na.rm = TRUE)
}

#' Predictive R^2 for out-of-sample evaluation using only t-1 information.
predictive_r2 <- function(r_actual, r_pred) {
  total_r2(r_actual, r_pred)
}

#' Sharpe ratio of a factor-mimicking portfolio's return series.
sharpe_ratio <- function(returns, periods_per_year = 12) {
  mu <- mean(returns, na.rm = TRUE)
  sigma <- sd(returns, na.rm = TRUE)
  (mu / sigma) * sqrt(periods_per_year)
}

#' Average absolute pricing error (alpha) across assets, given realized and
#' fitted return matrices (assets in columns).
avg_abs_alpha <- function(r, rhat) {
  alpha <- colMeans(r - rhat, na.rm = TRUE)
  mean(abs(alpha), na.rm = TRUE)
}
