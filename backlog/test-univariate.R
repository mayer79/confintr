
test_that("ci_quantile works", {
  x <- 1:27
  expect_equal(ci_quantile(x, q = 0.8)$estimate, quantile(x, 0.8, names = FALSE))
  expect_equal(ci_quantile(x, q = 0.4)$interval, c(6, 17))
  expect_equal(ci_quantile(x, q = 0.4)$interval <= ci_quantile(x, q = 0.5)$interval,
               c(TRUE, TRUE))
  expect_equal(ci_quantile(x, q = 0.4, type = "bootstrap", R = 249, seed = 2)$interval, c(7.0, 15.4))
  expect_equal(ci_quantile(x)$interval[2], ci_quantile(x, probs = c(0, 0.975))$interval[2])
  expect_equal(ci_quantile(x, R = 249, seed = 1, type = "bootstrap", boot_type = "perc")$interval[1],
               ci_quantile(x, R = 249, seed = 1, type = "bootstrap", boot_type = "perc", probs = c(0.025, 1))$interval[1])
  expect_equal(ci_quantile(x, R = 249, seed = 1, type = "bootstrap", boot_type = "norm")$interval[1],
               ci_quantile(x, R = 249, seed = 1, type = "bootstrap", probs = c(0.025, 1), boot_type = "norm")$interval[1])
})






