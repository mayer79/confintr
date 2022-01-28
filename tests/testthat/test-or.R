test_that("ci_oddsratio works", {
  x <- cbind(c(10, 5), c(4, 4))
  expect_equal(ci_oddsratio(x)$interval,
               c(0.2457839, 15.9953738), tolerance = 0.001)
  expect_equal(ci_oddsratio(x)$interval,
               as.numeric(fisher.test(x)$conf.int))
  expect_equal(ci_oddsratio(x, probs = c(0, 0.9))$interval,
               as.numeric(fisher.test(x, alternative = "less", conf.level = 0.9)$conf.int))
  expect_equal(ci_oddsratio(x, probs = c(0.05, 1))$interval,
               as.numeric(fisher.test(x, alternative = "greater", conf.level = 0.95)$conf.int))
  x <- cbind(c(10, 5), c(4, 0))
  expect_equal(ci_oddsratio(x)$interval,
               as.numeric(fisher.test(x)$conf.int))
  x <- cbind(c(10, 5), c(0, 4))
  expect_equal(ci_oddsratio(x)$interval,
               as.numeric(fisher.test(x)$conf.int))

})

