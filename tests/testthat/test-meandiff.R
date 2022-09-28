test_that("parametric confidence intervals for mean difference work", {
  set.seed(1)
  x <- runif(20)
  y <- runif(15)
  expect_equal(ci_mean_diff(x, y)$estimate, mean(x) - mean(y))
  expect_equal(ci_mean_diff(x, y)$interval, as.numeric(t.test(x, y)$conf.int))
  expect_equal(ci_mean_diff(x, y, probs = c(0.05, 1))$interval[1],
               as.numeric(t.test(x, y, alternative = "greater")$conf.int)[1])
  expect_equal(ci_mean_diff(x, y, probs = c(0, 0.95))$interval[2],
               as.numeric(t.test(x, y, alternative = "less")$conf.int)[2])
  expect_equal(ci_mean_diff(x, y, var.equal = TRUE)$interval,
               as.numeric(t.test(x, y, var.equal = TRUE)$conf.int))
  expect_equal(ci_mean_diff(x, y)$interval, c(-0.09317555, 0.30057542), tolerance = 0.001)
})

test_that("bootstrap confidence intervals for mean difference work", {
  set.seed(1)
  x <- runif(20)
  y <- runif(15)
  expect_equal(ci_mean_diff(x, y, type = "bootstrap", seed = 1, R = 455)$interval,
               c(-0.1053810, 0.3105886), tolerance = 0.001)
  expect_equal(ci_mean_diff(x, y, type = "bootstrap", boot_type = "bca", seed = 1, R = 455)$interval,
               c(-0.07918491, 0.29466186), tolerance = 0.001)
  expect_equal(ci_mean_diff(x, y, probs = c(0.025, 1), type = "bootstrap", seed = 1, R = 455)$interval[1],
               -0.105381, tolerance = 0.001)
  expect_equal(ci_mean_diff(x, y, probs = c(0, 0.975), type = "bootstrap", seed = 1, R = 455)$interval[2],
               0.3105886, tolerance = 0.001)
})
