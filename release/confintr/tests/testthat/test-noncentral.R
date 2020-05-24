context("CIs about non-centrality parameters")

test_that("confidence intervals for ncp of chi2 work", {
  tab <- table(mtcars[c("am", "vs")])
  chi <- chisq.test(tab, correct = FALSE)
  expect_equal(ci_chisq_ncp(chi)$interval, c(0, 8.475743), tolerance = 0.001)
  expect_equal(ci_chisq_ncp(chi, probs = c(0, 0.95))$interval[2], 6.735455, tolerance = 0.001)
})

test_that("confidence intervals for Cramer's V work", {
  tab <- table(mtcars[c("am", "vs")])
  chi <- chisq.test(tab, correct = FALSE)
  expect_equal(ci_cramersv(chi)$estimate, cramersv(chi))
  expect_equal(ci_cramersv(chi)$interval, c(0.1767767, 0.5441663), tolerance = 0.001)
  expect_equal(ci_cramersv(chi, probs = c(0, 0.95))$interval[2], 0.4916635, tolerance = 0.001)
  expect_equal(ci_cramersv(chi, probs = c(0.05, 1))$interval[1], 0.1767767, tolerance = 0.001)
  expect_equal(ci_cramersv(chi, type = "bootstrap", R = 499, seed = 1)$interval, c(0.1767767, 0.5), tolerance = 0.001)
})

test_that("confidence intervals for ncp of F work", {
  set.seed(100)
  x <- rnorm(100)
  y <- x + rnorm(100)
  fit <- lm(y~x)
  expect_equal(ci_f_ncp(fit)$interval, c(79.37824, 195.2245), tolerance = 0.001)
  expect_equal(ci_f_ncp(fit, probs = c(0, 0.95))$interval[2], 183.8974, tolerance = 0.001)
  expect_equal(ci_f_ncp(fit, probs = c(0.05, 1))$interval[1], 86.71208, tolerance = 0.001)
})

test_that("confidence intervals for R-squared work", {
  set.seed(100)
  x <- rnorm(100)
  y <- x + rnorm(100)
  fit <- lm(y~x)
  expect_equal(ci_rsquared(fit)$estimate, summary(fit)$r.squared, tolerance = 0.001)
  expect_equal(ci_rsquared(fit)$interval, c(0.4425188, 0.6612747), tolerance = 0.001)
  expect_equal(ci_rsquared(fit, probs = c(0, 0.95))$interval[2], 0.6477601, tolerance = 0.001)
  expect_equal(ci_rsquared(fit, probs = c(0.05, 1))$interval[1], 0.464416, tolerance = 0.001)
  expect_equal(ci_rsquared(7.470279, 2, 102, probs = c(0.05, 1))$interval, c(0.03552725, 1), tolerance = 0.001)
})

