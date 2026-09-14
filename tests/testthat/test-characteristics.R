test_that("mom_12_1 skips the most recent month correctly", {
  # 13 months of prices, monotonically increasing by a constant factor
  price <- 100 * (1.01)^(0:399)  # ~daily-freq proxy, 400 obs
  m <- mom_12_1(price, days_per_month = 21)
  # Should be NA for the first 12*21 observations, defined after that
  expect_true(all(is.na(m[1:(12 * 21)])))
  expect_true(any(!is.na(m)))
})

test_that("amihud_illiquidity is non-negative where defined", {
  set.seed(1)
  price <- cumprod(1 + rnorm(100, 0, 0.01)) * 100
  volume <- runif(100, 1e5, 1e6)
  illiq <- amihud_illiquidity(price, volume, 20)
  expect_true(all(illiq[!is.na(illiq)] >= 0))
})

test_that("roll_mean matches base R rolling calc on a known window", {
  x <- 1:10
  rm <- roll_mean(x, 3)
  expect_equal(rm[3], mean(1:3))
  expect_equal(rm[10], mean(8:10))
})
