context("Confidence intervals for correlations")

test_that("ci_cor works", {
  x <- 1:24
  expect_equal(ci_var(x)$estimate, var(x))
  expect_equal(ci_var(x)$interval, c(30.20305, 98.38687), tol = 0.001)
  expect_equal(ci_var(x, type = "bootstrap", R = 499, seed = 1)$interval, c(33.63669, 71.12911), tol = 0.001)
  expect_equal(ci_var(x)$interval[2], ci_var(x, probs = c(0, 0.975))$interval[2])
  expect_equal(ci_var(x, R = 249, seed = 1, type = "bootstrap", boot_type = "perc")$interval[1],
               ci_var(x, R = 249, seed = 1, type = "bootstrap", boot_type = "perc", probs = c(0.025, 1))$interval[1])
  expect_equal(ci_var(x, R = 249, seed = 1, type = "bootstrap", boot_type = "norm")$interval[1],
               ci_var(x, R = 249, seed = 1, type = "bootstrap", probs = c(0.025, 1), boot_type = "norm")$interval[1])
})

