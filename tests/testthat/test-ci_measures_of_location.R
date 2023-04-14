x <- exp(seq(0, 1, by = 0.01))
xp <- c(rep(0, 10), rep(1, 20))

test_that("ci_mean() works", {
  out <- ci_mean(x)

  expect_equal(out$estimate, mean(x))
  expect_equal(out$interval, c(stats::t.test(x)$conf.int))

  expect_true(out$interval[1L] < ci_mean(x, type = "Wald")$interval[1L])
  expect_true(out$interval[2L] > ci_mean(x, type = "Wald")$interval[2L])

  expect_equal(out$interval[1L], ci_mean(x, probs = c(0.025, 1))$interval[1L])
  expect_equal(out$interval[2L], ci_mean(x, probs = c(0, 0.975))$interval[2L])

  expect_true(out$interval[1L] < ci_mean(x, probs = c(0.05, 0.95))$interval[1L])
  expect_true(out$interval[2L] > ci_mean(x, probs = c(0.05, 0.95))$interval[2L])

  expect_no_error(ci_mean(x, type = "bootstrap", boot_type = "perc", R = 99))
})

test_that("ci_median() works", {
  out <- ci_median(x)

  expect_equal(out$estimate, stats::median(x))
  expect_equal(out$interval, ci_quantile(x, q = 0.5)$interval)

  expect_equal(out$interval[1L], ci_median(x, probs = c(0.025, 1))$interval[1L])
  expect_equal(out$interval[2L], ci_median(x, probs = c(0, 0.975))$interval[2L])

  expect_true(out$interval[1L] <= ci_median(x, probs = c(0.05, 0.95))$interval[1L])
  expect_true(out$interval[2L] >= ci_median(x, probs = c(0.05, 0.95))$interval[2L])

  expect_no_error(ci_median(x, type = "bootstrap", boot_type = "perc", R = 99))
})

test_that("ci_quantile() works", {
  out <- ci_quantile(x, q = 0.4)

  expect_equal(out$estimate, stats::quantile(x, probs = 0.4, names = FALSE))

  expect_equal(
    out$interval[1L], ci_quantile(x, q = 0.4, probs = c(0.025, 1))$interval[1L]
  )
  expect_equal(
    out$interval[2L], ci_quantile(x, q = 0.4, probs = c(0, 0.975))$interval[2L]
  )

  expect_true(
    out$interval[1L] <= ci_quantile(x, q = 0.4, probs = c(0.05, 0.95))$interval[1L]
  )
  expect_true(
    out$interval[2L] >= ci_quantile(x, q = 0.4, probs = c(0.05, 0.95))$interval[2L]
  )

  expect_true(out$interval[1L] <= ci_quantile(x, q = 0.5)$interval[1L])
  expect_true(out$interval[2L] >= ci_quantile(x, q = 0.3)$interval[2L])

  expect_no_error(
    ci_quantile(x, q = 0.4, type = "bootstrap", boot_type = "perc", R = 99)
  )
})

test_that("resulting object is complete", {
  comps <- c("parameter", "interval", "estimate", "probs", "type", "info")

  expect_equal(names(ci_mean(x)), comps)
  expect_equal(names(ci_mean(x, type = "boot", boot_type = "perc", R = 99)), comps)

  expect_equal(names(ci_mean(x, type = "boot", boot_type = "perc", R = 99)), comps)

  expect_equal(names(ci_median(x)), comps)
  expect_equal(names(ci_median(x, type = "boot", boot_type = "perc", R = 99)), comps)

  expect_equal(names(ci_quantile(x, q = 0.4)), comps)
  expect_equal(
    names(ci_quantile(x, q = 0.4, type = "boot", boot_type = "perc", R = 99)), comps
  )
})
