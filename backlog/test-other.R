

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
