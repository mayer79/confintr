test_that("standard errors for the mean work", {
  expect_equal(se_mean(1:10), t.test(1:10)$stderr)
  expect_error(se_mean(NA))
  expect_error(se_mean(c()))
  expect_no_error(se_mean(c(1, 0, NA)))
})

test_that("standard errors for the mean difference work", {
  expect_equal(se_mean_diff(1:3, 1:10), t.test(1:3, 1:10)$stderr)
  expect_equal(
    se_mean_diff(1:3, 1:10, var.equal = TRUE),
    t.test(1:3, 1:10, var.equal = TRUE)$stderr
  )
  expect_error(se_mean_diff(c(NA), 1:10))
  expect_error(se_mean_diff(1:3, c(NA)))
  expect_no_error(se_mean_diff(c(1:3, NA), c(1:10, NA)))
})

test_that("standard errors for the proportion work", {
  expect_equal(se_proportion(0:1), 0.5 / sqrt(2))
  expect_error(se_proportion(NA))
  expect_error(se_mean(c()))
  expect_no_error(se_mean(c(1, 0, NA)))
})

test_that("standard errors for the variance work", {
  # TODO: Need good specific example
  expect_equal(se_var(1:10), 2.356, tolerance = 1e-4)
  expect_error(se_var(c(NA, NA, 1:2)))
  expect_error(se_var(1:3))
})
