x <- 1:20

test_that("The first moments agree with usual statistics", {
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

test_that("skewness() agrees with DescTools::Skew(method = 1L)", {
  expect_equal(skewness(x), 0)
  expect_equal(skewness(c(x, NA), na.rm = TRUE), skewness(x))
  expect_equal(skewness(c(x, NA), na.rm = FALSE), NA_real_)

  xx <- seq(0, 1, by = 0.01)^2
  expect_equal(
    skewness(xx),
    0.6440406,  ## DescTools::Skew(xx, method = 1L)
    tolerance = 1e-5
  )
})

test_that("kurtosis() agrees with DescTools::Kurt() up to excess 3", {
  expect_true(kurtosis(x) < 3)
  expect_equal(kurtosis(c(x, NA), na.rm = TRUE), kurtosis(x))
  expect_equal(kurtosis(c(x, NA), na.rm = FALSE), NA_real_)

  xx <- seq(0, 1, by = 0.01)^2
  expect_equal(
    kurtosis(xx) - 3,
    -0.8517381,  ## DescTools::Kurt(xx, method = 1L)
    tolerance = 1e-5
  )
})

test_that("ci_skewness/kurtosis() give consistent estimates", {
  expect_equal(ci_skewness(x, boot_type = "perc", R = 99L)$estimate, skewness(x))
  expect_equal(ci_kurtosis(x, boot_type = "perc", R = 99L)$estimate, kurtosis(x))
})

test_that("CIs give consistent one- and two-sided intervals", {
  for (ci in c(ci_skewness, ci_kurtosis)) {
    out <- ci(x, boot_type = "perc", R = 99L, seed = 1L, probs = c(0.1, 0.8))$interval
    outl <- ci(x, boot_type = "perc", R = 99L, seed = 1L, probs = c(0.1, 1))$interval[1L]
    outr <- ci(x, boot_type = "perc", R = 99L, seed = 1L, probs = c(0, 0.8))$interval[2L]

    expect_equal(out[1L], outl)
    expect_equal(out[2L], outr)
  }
})

test_that("resulting object is complete", {
  comps <- c("parameter", "interval", "estimate", "probs", "type", "info")
  expect_equal(names(ci_skewness(x, boot_type = "perc", R = 99L)), comps)
  expect_equal(names(ci_kurtosis(x, boot_type = "perc", R = 99L)), comps)
})
