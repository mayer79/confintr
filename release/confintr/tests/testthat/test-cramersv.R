context("Cramer's V")

test_that("different input leads to same and correct result", {
  tab <- table(mtcars[c("am", "vs")])
  chi <- chisq.test(tab, correct = FALSE)

  expect_equal(cramersv(chi), cramersv(tab))
  expect_equal(cramersv(chi), cramersv(mtcars[c("am", "vs")]))
  expect_equal(cramersv(chi), 0.1683451, tolerance = 1e-6)
})

test_that("wrong input leads to error", {
  expect_error(cramersv(1:100))
  expect_error(cramersv(cbind(1:2, -1:0)))
})
