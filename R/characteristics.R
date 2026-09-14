# R/characteristics.R
# Helper functions for constructing firm-characteristic proxies from
# price/volume series only (no fundamentals data source wired up yet).

#' Rolling mean, right-aligned, NA-padded at the start
roll_mean <- function(x, n) {
  zoo::rollapply(x, width = n, FUN = mean, fill = NA, align = "right")
}

#' Rolling standard deviation of simple returns, right-aligned
roll_sd_return <- function(price, n) {
  ret <- price / dplyr::lag(price) - 1
  zoo::rollapply(ret, width = n, FUN = sd, fill = NA, align = "right", na.rm = TRUE)
}

#' 12-month return skipping the most recent month (standard momentum signal)
mom_12_1 <- function(price, days_per_month = 21) {
  lag_1m  <- dplyr::lag(price, days_per_month)
  lag_12m <- dplyr::lag(price, 12 * days_per_month)
  lag_1m / lag_12m - 1
}

#' Amihud (2002) illiquidity: rolling mean of |return| / dollar volume
amihud_illiquidity <- function(price, volume, n) {
  ret <- price / dplyr::lag(price) - 1
  dollar_vol <- price * volume
  illiq_daily <- abs(ret) / dollar_vol
  roll_mean(illiq_daily, n)
}
