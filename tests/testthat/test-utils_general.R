test_that("is.cint() works", {
  expect_true(is.cint(ci_mean(1:10)))
  expect_false(is.cint(c(1, 10)))
})

test_that("print() does not give an error", {
  capture.output(expect_no_error(print(ci_mean(1:10))))
})

test_that("check_probs() gives errors for bad input", {
  expect_error(check_probs(0.5))
  expect_error(check_probs(c(0.3, 0.2)))
  expect_error(check_probs(c(-1, 0.9)))
  expect_error(check_probs(c(0.1, 2)))
  expect_error(check_probs(0:1))
  expect_error(check_probs(c(0.1, NA)))
  expect_error(check_probs(LETTERS[1:2]))

  expect_true(check_probs(c(0.1, 0.9)))
  expect_true(check_probs(c(0.01, 0.9)))
})

test_that("check_output() works", {
  expect_error(check_output(c(NA, 1), probs = c(0.1, 0.9)))
  expect_error(check_output(1, probs = c(0.1, 0.9)))

  expect_no_error(check_output(0:1, probs = c(0.1, 0.9)))
  expect_no_error(check_output(c(-Inf, Inf), probs = c(0.1, 0.9)))

  r <- c(0, 20)
  expect_equal(check_output(c(2, 4), probs = c(0.1, 0.9), parameter_range = r), c(2, 4))
  expect_equal(check_output(c(2, 40), probs = c(0.1, 0.9), parameter_range = r), c(2, 20))
  expect_equal(check_output(c(-1, 10), probs = c(0.1, 0.9), parameter_range = r), c(0, 10))
  expect_equal(check_output(c(-1, 30), probs = c(0.1, 0.9), parameter_range = r), r)

  # Maybe unexpected?
  expect_equal(check_output(c(1, 2), probs = c(0.1, 1), parameter_range = r), c(1, 20))
  expect_equal(check_output(c(1, 2), probs = c(0.1, 1), parameter_range =  c(0, Inf)), c(1, Inf))
})

test_that("set_seed() works as expected", {
  expect_equal({set_seed(1L); rnorm(1L)}, {set_seed(1L); rnorm(1L)})
  expect_false({set_seed(NULL); rnorm(1L)} == {set_seed(NULL); rnorm(1L)})
})

test_that("format_p() works as expected (behavior not fully clear with digits1=1)", {
  expect_equal(format_p(0.111, digits = 3), "11.1%")
  expect_equal(format_p(0.111, digits = 2), "11%")
  expect_equal(format_p(0.111, digits = 1), "11%")
})

test_that("is_equal_tailed() works as expected", {
  expect_equal(is_equal_tailed(c(0.1, 1 - 0.1)), TRUE)
  expect_equal(is_equal_tailed(c(0.1, 1 - 0.05)), FALSE)
  expect_equal(is_equal_tailed(c(0.1, 1)), FALSE)
})

test_that("is_onesided() works as expected", {
  expect_equal(is_onesided(c(0.1, 1 - 0.1)), FALSE)
  expect_equal(is_onesided(c(0, 0.99)), TRUE)
  expect_equal(is_onesided(c(0.1, 1)), TRUE)
})

test_that("unequal_stop() always gives error", {
  expect_error(unequal_stop())
})

test_that("props2text() works", {
  expect_equal(props2text(c(0.1, 0.9)), "Two-sided")
  expect_equal(props2text(c(0.2, 0.9)), "Unequal-tailed two-sided")
  expect_equal(props2text(c(0, 0.99)), "One-sided")
})

test_that("probs2alternative() works", {
  expect_error(probs2alternative(c(0.1, 0.99)))

  expect_equal(probs2alternative(c(0.1, 0.9)), "two.sided")
  expect_equal(probs2alternative(c(0, 0.9)), "less")
  expect_equal(probs2alternative(c(1, 0.01)), "greater")
})

