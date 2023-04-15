x <- cbind(c(10, 5), c(4, 4))
X <- as.data.frame.table(x)
X <- X[rep(1:4, X$Freq), 1:2]

test_that("or_align_input() works", {
  expect_equal(or_align_input(X), x)

  expect_error(or_align_input(1))
  expect_error(or_align_input(iris))
  expect_error(or_align_input(cbind(x, c(1:2))))
  expect_error(or_align_input(cbind(1:2, -1:0)))
})

test_that("oddsratio() agrees with manual calculation", {
  expect_equal(oddsratio(x), 2)
  expect_equal(oddsratio(x), oddsratio(X))
})

test_that("ci_oddsratio() gives the same estimate as oddsratio()", {
  expect_equal(ci_oddsratio(x)$estimate, oddsratio(x))
})

test_that("ci_oddsratio() agrees with stats::fisher.test()", {
  expect_equal(ci_oddsratio(x)$interval, as.numeric(stats::fisher.test(x)$conf.int))
  expect_equal(
    ci_oddsratio(x, probs = c(0, 0.9))$interval,
    c(stats::fisher.test(x, alternative = "less", conf.level = 0.9)$conf.int)
  )
  expect_equal(
    ci_oddsratio(x, probs = c(0.05, 1))$interval,
    c(stats::fisher.test(x, alternative = "greater", conf.level = 0.95)$conf.int)
  )

  expect_error(ci_oddsratio(x, probs = c(0.025, 0.1)))
})

test_that("resulting object is complete", {
  comps <- c("parameter", "interval", "estimate", "probs", "type", "info")
  expect_equal(names(ci_oddsratio(x)), comps)
})
