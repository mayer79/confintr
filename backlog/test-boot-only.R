
test_that("ci_quantile_diff works", {
  x <- 1:70
  y <- 1:20
  expect_equal(ci_quantile_diff(x, y, R = 99, seed = 1, q = 0.2)$estimate,
               quantile(x, 0.2, names = FALSE) - quantile(y, 0.2, names = FALSE))
  expect_equal(ci_quantile_diff(x, y, R = 99, seed = 1)$interval, c(15.99780, 35.41227), tolerance = 0.001)
  expect_equal(ci_quantile_diff(x, y, R = 99, seed = 1)$interval >
               ci_quantile_diff(x, y, R = 99, seed = 1, q = 0.25)$interval, c(TRUE, TRUE))
  expect_equal(ci_quantile_diff(x, y, R = 99, seed = 1, probs = c(0.1, 0.9))$interval <
                 ci_quantile_diff(x, y, R = 99, seed = 1, probs = c(0.1, 0.9),
                                  q = 0.75)$interval, c(TRUE, TRUE))
  expect_equal(ci_quantile_diff(x, y, R = 99, seed = 1)$interval[2],
               ci_quantile_diff(x, y, R = 99, seed = 1, probs = c(0, 0.975))$interval[2])
  expect_equal(ci_quantile_diff(x, y, R = 99, seed = 1, boot_type = "perc")$interval[1],
               ci_quantile_diff(x, y, R = 99, seed = 1, boot_type = "perc", probs = c(0.025, 1))$interval[1])
  expect_equal(ci_quantile_diff(x, y, R = 99, seed = 1, boot_type = "norm")$interval[1],
               ci_quantile_diff(x, y, R = 99, seed = 1, probs = c(0.025, 1), boot_type = "norm")$interval[1])
})

test_that("ci_median_diff works", {
  set.seed(1)
  x <- runif(10)
  y <- runif(10)
  expect_equal(ci_median_diff(x, y, R = 499, seed = 1, probs = c(0.2, 0.8))$estimate,
               median(x) - median(y))
  expect_equal(ci_median_diff(x, y, R = 499, seed = 1)$interval,
               ci_quantile_diff(x, y, R = 499, seed = 1)$interval)
  expect_equal(ci_median_diff(x, y, R = 499, seed = 1, boot_type = "perc")$interval,
               ci_quantile_diff(x, y, R = 499, seed = 1, boot_type = "perc")$interval)
})
