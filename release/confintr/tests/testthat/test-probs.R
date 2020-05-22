context("probs")

test_that("coverage should be within 0 and 1", {
  x <- runif(15)
  expect_error(ci_mean(x, probs = c(0, 1)))
  expect_error(ci_sd(x, probs = c(-0.1, 1)))
  expect_error(ci_median(x, probs = c(0.1, 1.1)))
  expect_error(ci_var(x, probs = c(0.9, 0.1)))
})
