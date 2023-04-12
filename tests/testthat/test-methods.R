test_that("is.cint() works", {
  expect_true(is.cint(ci_mean(1:10)))
  expect_false(is.cint(c(1, 10)))
})

test_that("print() does not give an error", {
  capture.output(expect_no_error(print(ci_mean(1:10))))
})

