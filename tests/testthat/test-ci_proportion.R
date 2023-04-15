X <- c(rep(0, 10), rep(1, 20))
x <- sum(X)
n <- length(X)

test_that("the API works", {
  expect_error(ci_proportion(x))
  expect_error(ci_proportion(x, n = 1L))
  expect_no_error(ci_proportion(X, n = 1L))
  expect_error(ci_proportion(0:10))
  expect_error(ci_proportion("A"))

  expect_equal(ci_proportion(X), ci_proportion(x, n))
})

test_that("estimate is correct", {
  expect_equal(ci_proportion(X)$estimate, mean(X))
})

test_that("Clopper-Pearson CI agrees with stats::binom.test()", {
  expect_equal(
    ci_proportion(X)$interval, as.vector(stats::binom.test(x, n = n)$conf.int)
  )
  expect_equal(
    ci_proportion(X, probs = c(0.05, 0.95))$interval,
    as.vector(stats::binom.test(x, n = n, conf.level = 0.9)$conf.int)
  )
  expect_equal(
    ci_proportion(X, probs = c(0, 0.95))$interval,
    as.vector(stats::binom.test(x, n = n, alternative = "less")$conf.int)
  )
  expect_equal(
    ci_proportion(X, probs = c(0.025, 1))$interval,
    as.vector(
      stats::binom.test(x, n = n, alternative = "greater", conf.level = 0.975)$conf.int
    )
  )
  expect_error(ci_proportion(X, probs = c(0.025, 0.9)))
})

test_that("Wilson's score CI agrees with binom::(..., method = 'wilson')", {
  expect_equal(
    ci_proportion(x, n, type = "Wilson")$interval,
    c(0.4878005, 0.807695),  # binom::binom.confint(x, n, method = "wilson")
    tolerance = 1e-6
  )

  expect_equal(
    ci_proportion(x, n, type = "Wilson", probs = c(0.1, 0.9))$interval,
    c(0.5502703, 0.7657616),  # binom::binom.confint(x, n, conf.level = 0.8, method = "wilson")
    tolerance = 1e-6
  )
})

test_that("Agresti-Coull agrees with binom::(..., method = 'ac')", {
  expect_equal(
    ci_proportion(x, n, type = "Agresti-Coull")$interval,
    c(0.4868117, 0.8086838),  # binom::binom.confint(x, n, method = "ac")
    tolerance = 1e-6
  )

  expect_equal(
    ci_proportion(x, n, type = "Agresti-Coull", probs = c(0.1, 0.9))$interval,
    c(0.5499416, 0.7660903),  # binom::binom.confint(x, n, conf.level = 0.8, method = "ac")
    tolerance = 1e-6
  )
})

test_that("bootstrap agrees with bootstrap CI for the mean (not such a good check)", {
  expect_no_error(
    out <- ci_proportion(
      X, probs = c(0.05, 0.9), type = "boot", boot_type = "perc", R = 99, seed = 1L
    )
  )
  expect_equal(
    out$interval,
    ci_mean(
      X, probs = c(0.05, 0.9), type = "boot", boot_type = "perc", R = 99, seed = 1L
    )$interval
  )
})

test_that("CIs give consistent one- and two-sided intervals", {
  for (t in c("Wilson", "Agresti-Coull", "bootstrap")) {
    out <- ci_proportion(
      X, type = t, boot_type = "perc", R = 99L, seed = 1L, probs = c(0.1, 0.9)
    )$interval
    outl <- ci_proportion(
      X, type = t, boot_type = "perc", R = 99L, seed = 1L, probs = c(0.1, 1)
    )$interval[1L]
    outr <- ci_proportion(
      X, type = t, boot_type = "perc", R = 99L, seed = 1L, probs = c(0, 0.9)
    )$interval[2L]

    expect_equal(out[1L], outl)
    expect_equal(out[2L], outr)
  }
})

test_that("Parametric CI cannot deal with unequal-tailed probs", {
  expect_error(ci_proportion(X, probs = c(0.05, 0.9), type = "Wilson"))
  expect_error(ci_proportion(X, probs = c(0.05, 0.9), type = "Agresti-Coull"))
})

test_that("resulting object is complete", {
  comps <- c("parameter", "interval", "estimate", "probs", "type", "info")

  expect_equal(names(ci_proportion(X)), comps)
  expect_equal(names(ci_proportion(X, type = "Wilson")), comps)
  expect_equal(names(ci_proportion(X, type = "Agresti-Coull")), comps)
  expect_equal(
    names(ci_proportion(X, type = "boot", boot_type = "stud", R = 99L)), comps
  )
})
