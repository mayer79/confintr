test_that("confidence intervals for ncp of chi2 work", {
  tab <- table(mtcars[c("am", "vs")])
  chi <- chisq.test(tab, correct = FALSE)
  expect_equal(ci_chisq_ncp(chi)$interval, c(0, 8.475743), tolerance = 0.001)
  expect_equal(ci_chisq_ncp(chi, probs = c(0, 0.95))$interval[2], 6.735455, tolerance = 0.001)
})

test_that("confidence intervals for Cramer's V work", {
  tab <- table(mtcars[c("am", "vs")])
  chi <- chisq.test(tab, correct = FALSE) # p-value = 0.3409
  expect_equal(ci_cramersv(chi)$estimate, 0.1683451, tolerance = 0.001)
  expect_equal(ci_cramersv(chi)$estimate, cramersv(chi))
  expect_equal(ci_cramersv(chi)$interval, c(0, 0.5441663), tolerance = 0.001)
  expect_equal(ci_cramersv(chi, probs = c(0, 0.95))$interval[2], 0.4916635, tolerance = 0.001)
  expect_equal(ci_cramersv(chi, probs = c(0.05, 1))$interval[1], 0, tolerance = 0.001)
  expect_equal(ci_cramersv(chi, type = "bootstrap", R = 499, seed = 1)$interval, c(0, 0.5), tolerance = 0.001)

  # Correspondence with chi-squared test
  tab <- tab * 4
  chi <- chisq.test(tab, correct = FALSE) # p-value = 0.05683
  expect_equal(ci_cramersv(chi, probs = c(0.05, 1))$interval[1], 0)
  tab <- tab * 1.1
  chi <- chisq.test(tab, correct = FALSE) # p-value = 0.04576
  expect_equal(ci_cramersv(chi, probs = c(0.05, 1))$interval[1], 0.08590156, tolerance = 0.001)
})
