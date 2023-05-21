fit <- lm(Sepal.Width ~ ., data = iris)
fstat <- summary(fit)$fstat
stat <- fstat[["value"]]
df1 <- fstat[["numdf"]]
df2 <- fstat[["dendf"]]
r2 <- summary(fit)$r.squared

test_that("the input must make sense", {
  expect_error(r2_align_input(4))
  expect_error(r2_align_input(4, df1 = 1))
  expect_error(r2_align_input(4, df2 = 1))
  expect_error(r2_align_input(1:2, df1 = 1, df2 = 1))
})

test_that("the two APIs give identical output", {
  expect_equal(r2_align_input(fit), r2_align_input(stat, df1, df2))
  expect_equal(ci_rsquared(fit), ci_rsquared(stat, df1, df2))
  expect_equal(ci_f_ncp(fit), ci_f_ncp(stat, df1, df2))
})

test_that("estimate of R2 obtained via ci_rsquared() agrees with usual estimates", {
  expect_equal(ci_rsquared(fit)$estimate, r2)
})

test_that("ci_rsquared() agrees with MBESS::ci.R2()", {
  # Package version ‘4.9.2
  # MBESS::ci.R2(r2, df.1 = df1, df.2 = df2, Random.Predictors = FALSE)
  expect_equal(ci_rsquared(fit)$interval, c(0.5295201, 0.6926153), tolerance = 1e-5)

  # MBESS::ci.R2(r2, df.1 = df1, df.2 = df2, Random.Predictors = FALSE, alpha.lower = 0, alpha.upper = 0.1)
  expect_equal(
    ci_rsquared(fit, probs = c(0, 0.9))$interval, c(0, 0.6697937), tolerance = 1e-5
  )

  # MBESS::ci.R2(r2, df.1 = df1, df.2 = df2, Random.Predictors = FALSE, alpha.lower = 0.1, alpha.upper = 0)
  expect_equal(
    ci_rsquared(fit, probs = c(0.1, 1))$interval, c(0.563586, 1), tolerance = 1e-5
  )
})

test_that("ci_f_ncp() agrees with MBESS::conf.limits.ncf()", {
  # Package version ‘4.9.2
  # MBESS::conf.limits.ncf(stat, df.1 = df1, df.2 = df2)
  expect_equal(ci_f_ncp(fit)$interval, c(168.8234, 337.9878), tolerance = 1e-3)

  # MBESS::conf.limits.ncf(stat, df.1 = df1, df.2 = df2, alpha.lower = 0, alpha.upper = 0.1, conf.level = NULL)
  expect_equal(
    ci_f_ncp(fit, probs = c(0, 0.9))$interval, c(0, 304.2615), tolerance = 1e-3
  )

  # MBESS::conf.limits.ncf(stat, df.1 = df1, df.2 = df2, alpha.lower = 0.1, alpha.upper = 0, conf.level = NULL)
  expect_equal(
    ci_f_ncp(fit, probs = c(0.1, 1))$interval, c(193.7103, Inf), tolerance = 1e-3
  )
})

test_that("ci_*() give consistent one- and two-sided CIs", {
  for (ci in c(ci_rsquared, ci_f_ncp)) {
    out <- ci(fit, probs = c(0.1, 0.8))$interval
    outl <- ci(fit, probs = c(0.1, 1))$interval[1L]
    outr <- ci(fit, probs = c(0, 0.8))$interval[2L]

    expect_equal(out[1L], outl)
    expect_equal(out[2L], outr)
  }
})

test_that("resulting object is complete", {
  comps <- c("parameter", "interval", "estimate", "probs", "type", "info")
  expect_equal(names(ci_rsquared(fit)), comps)
  expect_equal(names(ci_f_ncp(fit)), comps)
})
