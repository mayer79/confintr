test_that("is.cint works", {
  expect_equal(is.cint(ci_mean(runif(100))), TRUE)
  expect_equal(is.cint("cint"), FALSE)
})

test_that("standard errors work", {
  expect_equal(se_mean(1:10), t.test(1:10)$stderr)
  expect_equal(se_proportion(0:1), 0.5 / sqrt(2))
  expect_equal(se_mean_diff(1:3, 1:3), t.test(1:3, 1:3)$stderr)
  expect_equal(se_mean_diff(1:3, 1:3, var.equal = TRUE),
               t.test(1:3, 1:3, var.equal = TRUE)$stderr)
  expect_equal(se_var(1:10), 2.356, tolerance = 1e-4)
  expect_error(se_var(1:3))
  expect_error(se_mean(NA))
  expect_error(se_proportion(NA))
  expect_error(se_mean_diff(1, 2))
})


test_that("moments works", {
  expect_equal(moment(1:100, central = FALSE), mean(1:100))
  expect_equal(moment(1:100), 0)
  expect_equal(skewness(1:100), 0)
  expect_equal(kurtosis(1:100), 1.79976, tolerance = 0.001)
})

test_that("check_probs works", {
  expect_error(check_probs(0, 1))
  expect_error(check_probs(-1, 0.9))
  expect_error(check_probs(0, 9))
  expect_error(check_probs(0.9, 0.1))
  expect_equal(check_probs(c(0.1, 0.9)), TRUE)
})

test_that("check_output works", {
  expect_error(check_output(3))
  expect_equal(check_output(c(0, 199), c(0.05, 1), 0:1), 0:1)
  expect_equal(check_output(c(0, 199), c(0.05, 0.9), 0:1), 0:1)
})

test_that("other helper functions work", {
  expect_equal(is_symmetric(c(0.1, 0.9)), TRUE)
  expect_equal(is_symmetric(c(0.1, 0.91)), FALSE)
  expect_equal(is_onesided(c(0.1, 0.91)), FALSE)
  expect_equal(is_onesided(c(0.1, 1)), TRUE)
 # expect_equal(zap_small(c(0.0001, 1), 0.002), 0:1)
  expect_error(check_bca("bca", 100, 99))
  expect_equal(check_bca("perc", 100, 99), TRUE)
  expect_equal(title_case1("hi michael"), "Hi michael")
  expect_equal(map_boot_type("norm"), "normal")
  expect_error(map_boot_type("hi"))
})
