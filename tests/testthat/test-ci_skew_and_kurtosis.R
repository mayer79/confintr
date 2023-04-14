x <- 1:20

test_that("moment() works", {
  expect_equal(moment(x, p = 1, central = FALSE), mean(x))
  expect_equal(moment(x, p = 1, central = TRUE), 0)
  expect_equal(moment(x, p = 2, central = FALSE), sum(x^2) / length(x))
  expect_equal(
    moment(x, p = 2, central = TRUE),
    stats::var(x) * (length(x) - 1L) / length(x)
  )
  expect_true(moment(c(1, 2, 100), p = 3) > 0)
  expect_equal(moment(c(x, NA), na.rm = TRUE), moment(x))
  expect_equal(moment(c(x, NA), na.rm = FALSE), NA_real_)
})

test_that("skewness() works", {
  expect_equal(skewness(x), 0)
  expect_equal(skewness(c(x, NA), na.rm = TRUE), skewness(x))
  expect_equal(skewness(c(x, NA), na.rm = FALSE), NA_real_)
})

test_that("kurtosis() works", {
  expect_true(kurtosis(x) < 3)
  expect_equal(kurtosis(c(x, NA), na.rm = TRUE), kurtosis(x))
  expect_equal(kurtosis(c(x, NA), na.rm = FALSE), NA_real_)
})

test_that("ci_skewness() works", {
  expect_no_error(out <- ci_skewness(x, R = 99L, boot_type = "perc"))
  expect_equal(out$estimate, skewness(x))
})

test_that("ci_kurtosis() works", {
  expect_no_error(out <- ci_kurtosis(x, R = 99L, boot_type = "perc"))
  expect_equal(out$estimate, kurtosis(x))
})

test_that("resulting object is complete", {
  comps <- c("parameter", "interval", "estimate", "probs", "type", "info")
  expect_equal(names(ci_skewness(x, boot_type = "perc", R = 99L)), comps)
  expect_equal(names(ci_kurtosis(x, boot_type = "perc", R = 99L)), comps)
})
