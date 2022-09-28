test_that("ci_var works", {
  x <- 1:24
  expect_equal(ci_var(x)$estimate, var(x))
  expect_equal(ci_var(x)$interval, c(30.20305, 98.38687), tolerance = 0.001)
  expect_equal(ci_var(x, type = "bootstrap", R = 499, seed = 1)$interval, c(33.63669, 71.12911), tolerance = 0.001)
  expect_equal(ci_var(x)$interval[2], ci_var(x, probs = c(0, 0.975))$interval[2])
  expect_equal(ci_var(x, R = 249, seed = 1, type = "bootstrap", boot_type = "perc")$interval[1],
               ci_var(x, R = 249, seed = 1, type = "bootstrap", boot_type = "perc", probs = c(0.025, 1))$interval[1])
  expect_equal(ci_var(x, R = 249, seed = 1, type = "bootstrap", boot_type = "norm")$interval[1],
               ci_var(x, R = 249, seed = 1, type = "bootstrap", probs = c(0.025, 1), boot_type = "norm")$interval[1])
})

test_that("ci_sd works", {
  x <- 1:24
  expect_equal(ci_sd(x)$estimate, sd(x))
  expect_equal(ci_sd(x)$interval, sqrt(ci_var(x)$interval))
  expect_equal(ci_sd(x)$interval, c(5.495730, 9.919015), tolerance = 0.001)
  expect_equal(ci_sd(x, probs = c(0, 0.975))$interval, c(0, 9.919015), tolerance = 0.001)
  expect_equal(ci_sd(x, probs = c(0.025, 1))$interval, c(5.49573, Inf), tolerance = 0.001)
  expect_equal(ci_sd(x, type = "bootstrap", R = 499, seed = 1)$interval, c(5.799715,8.433808), tolerance = 0.001)
  expect_equal(ci_sd(x)$interval[2], ci_sd(x, probs = c(0, 0.975))$interval[2])
  expect_equal(ci_sd(x, R = 249, seed = 1, type = "bootstrap", boot_type = "perc")$interval[1],
               ci_sd(x, R = 249, seed = 1, type = "bootstrap", boot_type = "perc", probs = c(0.025, 1))$interval[1])
  expect_equal(ci_sd(x, R = 249, seed = 1, type = "bootstrap", boot_type = "norm")$interval[1],
               ci_sd(x, R = 249, seed = 1, type = "bootstrap", probs = c(0.025, 1), boot_type = "norm")$interval[1])
  x <- rnorm(100)
  xp <- (x - mean(x)) / sd(x) * 8
  expect_equal(ci_sd(xp, probs = c(0, 0.95))$interval[2], 9.07, tolerance = 0.01)
})

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

test_that("ci_median works", {
  x <- 1:27
  expect_equal(ci_median(x)$estimate, median(x))
  expect_equal(ci_median(x)$interval, ci_quantile(x)$interval)
  expect_equal(ci_median(x)$interval, c(8, 20))
  expect_equal(ci_median(x, type = "bootstrap", R = 249, seed = 2)$interval, c(7.758812, 17.000000), tolerance = 0.0001)
  expect_equal(ci_median(x)$interval[2], ci_median(x, probs = c(0, 0.975))$interval[2])
  expect_equal(ci_median(x, R = 249, seed = 1, type = "bootstrap", boot_type = "perc")$interval[1],
               ci_median(x, R = 249, seed = 1, type = "bootstrap", boot_type = "perc", probs = c(0.025, 1))$interval[1])
  expect_equal(ci_median(x, R = 249, seed = 1, type = "bootstrap", boot_type = "norm")$interval[1],
               ci_median(x, R = 249, seed = 1, type = "bootstrap", probs = c(0.025, 1), boot_type = "norm")$interval[1])
})

test_that("ci_mean works", {
  x <- 1:27
  expect_equal(ci_mean(x)$estimate, mean(x))
  expect_equal(ci_mean(x)$interval, as.numeric(t.test(x)$conf.int))
  expect_equal(ci_mean(x)$interval, c(10.86013, 17.13987), tolerance = 0.001)
  expect_equal(c(ci_mean(x, probs = c(0.025, 1))$interval[1], ci_mean(x, probs = c(0, 0.975))$interval[2]),
               c(10.86013, 17.13987), tolerance = 0.001)
  expect_equal(ci_mean(x, type = "bootstrap", R = 249, seed = 2)$interval, c(11.09861, 16.99854), tolerance = 0.0001)
  expect_equal(ci_mean(x)$interval[2], ci_mean(x, probs = c(0, 0.975))$interval[2])
  expect_equal(ci_mean(x, R = 249, seed = 1, type = "bootstrap", boot_type = "perc")$interval[1],
               ci_mean(x, R = 249, seed = 1, type = "bootstrap", boot_type = "perc", probs = c(0.025, 1))$interval[1])
  expect_equal(ci_mean(x, R = 249, seed = 1, type = "bootstrap", boot_type = "norm")$interval[1],
               ci_mean(x, R = 249, seed = 1, type = "bootstrap", probs = c(0.025, 1), boot_type = "norm")$interval[1])
})


test_that("ci_proportion works", {
  x <- 10
  n <- 40
  expect_equal(ci_proportion(x, n)$estimate, as.numeric(binom.test(x, n)$estimate))
  expect_equal(ci_proportion(x, n)$interval, as.numeric(binom.test(x, n)$conf.int))
  expect_equal(ci_proportion(x, n, probs = c(0.05, 1))$interval,
               as.numeric(binom.test(x, n, alternative = "greater")$conf.int))
  expect_equal(ci_proportion(x, n, probs = c(0, 0.9))$interval,
               as.numeric(binom.test(x, n, conf.level = 0.9, alternative = "less")$conf.int))
  expect_equal(ci_proportion(x, n, type = "bootstrap", R = 249, seed = 2)$interval, c(0.1115616, 0.35), tolerance = 0.0001)
  expect_equal(ci_proportion(x, n, type = "Wilson")$interval, c(0.1418712, 0.4019396), tolerance = 0.0001)
  expect_equal(ci_proportion(x, n, type = "Agresti-Coull")$interval, c(0.1401985, 0.4036123), tolerance = 0.0001)
  expect_equal(ci_proportion(x, n, type = "Wilson")$interval[2],
               ci_proportion(x, n, type = "Wilson", probs = c(0, 0.975))$interval[2])
  expect_equal(ci_proportion(x, n, R = 249, seed = 1, type = "bootstrap", boot_type = "perc")$interval[1],
               ci_proportion(x, n, R = 249, seed = 1, type = "bootstrap", boot_type = "perc", probs = c(0.025, 1))$interval[1])
  expect_equal(ci_proportion(x, n, R = 249, seed = 1, type = "bootstrap", boot_type = "norm")$interval[1],
               ci_proportion(x, n, R = 249, seed = 1, type = "bootstrap", probs = c(0.025, 1), boot_type = "norm")$interval[1])
  expect_equal(ci_proportion(45, n = 50, probs = c(0, 0.95), type = "Wilson")$interval[2], 0.95047, tolerance = 0.001)
})


