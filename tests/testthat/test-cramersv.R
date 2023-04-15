X <- mtcars[c("am", "vs")]
tab <- table(X)
chi <- chisq.test(tab, correct = FALSE)   # always the case for Cramer's v
chi_c <- chisq.test(tab, correct = TRUE)  # the default for the NCP of the X2
df <- as.vector(chi$parameter)

test_that("the input must make sense", {
  expect_error(cramersv_align_input(mtcars))
  expect_error(cramersv_align_input(1))
  expect_error(cramersv_align_input(t.test(1:10)))
  expect_error(cramersv_align_input(cbind(1:2, -1:0)))
})

test_that("The two APIs give the same", {
  expect_equal(cramersv(chi), cramersv(tab))
  expect_equal(cramersv(chi), cramersv(X))

  expect_equal(ci_cramersv(chi), ci_cramersv(tab))
  expect_equal(ci_cramersv(chi), ci_cramersv(X))

  expect_equal(ci_chisq_ncp(chi), ci_chisq_ncp(tab, correct = FALSE))
  expect_equal(ci_chisq_ncp(chi), ci_chisq_ncp(X, correct = FALSE))

  expect_equal(ci_chisq_ncp(chi_c), ci_chisq_ncp(tab))
  expect_equal(ci_chisq_ncp(chi_c), ci_chisq_ncp(X))
})

test_that("cramersv() gives the same as rcompanion::cramerV()", {
  # Version ‘2.4.26’
  # rcompanion::cramerV(tab, digits = 7)
  expect_equal(cramersv(tab), 0.1683451, tolerance = 1e-6)
})

test_that("Estimate from ci_cramersv() agrees with cramersv()", {
  expect_equal(ci_cramersv(X)$estimate, cramersv(X))
})

test_that("ci_chisq_ncp() agrees with MBESS::conf.limits.nc.chisq()", {
  # Package version ‘4.9.2
  # MBESS::conf.limits.nc.chisq(chi$statistic, df = df)
  # Note that MBESS does not apply a Yates correction in the 2x2 case
  expect_equal(
    ci_chisq_ncp(X, correct = FALSE)$interval,
    c(0, 8.475743),
    tolerance = 1e-5
  )

  # MBESS::conf.limits.nc.chisq(chi$statistic, df = df, alpha.lower = 0, alpha.upper = 0.1, conf.level = NULL)
  expect_equal(
    ci_chisq_ncp(X, probs = c(0, 0.9), correct = FALSE)$interval,
    c(0, 4.97156),
    tolerance = 1e-5
  )

  # MBESS::conf.limits.nc.chisq(chi$statistic, df = df, alpha.lower = 0.1, alpha.upper = 0, conf.level = NULL)
  expect_equal(
    ci_chisq_ncp(X, probs = c(0.1, 1), correct = FALSE)$interval,
    c(0, Inf),
    tolerance = 1e-5
  )
})

test_that("ci_cramersv() is consistent with example in Smithson, p41", {
  test_scores <- as.table(
    rbind(
      Private = c(6, 14, 17, 9),
      Public = c(30, 32, 17, 3))
  )
  suppressWarnings(out <- ci_cramersv(test_scores))
  expect_equal(out$estimate, 0.368, tolerance = 0.003)
  expect_equal(out$interval, c(0.223, 0.545), tolerance = 0.003)
})

test_that("ci_cramersv() is consistent with significance in chisq.test", {
  tab4 <- tab * 4.23  # to be almost significant
  chi4 <- stats::chisq.test(tab4, correct = FALSE)  # p-value = 0.05016
  expect_true(ci_cramersv(tab4, probs = c(0.05, 1))$interval[1L] == 0)
  tab4p <- tab * 4.24
  chi4p <- stats::chisq.test(tab4p, correct = FALSE) # p-value = 0.04989
  expect_true(ci_cramersv(tab4p, probs = c(0.05, 1))$interval[1L] > 0)
})

# Correspondence with chi-squared test
tab <- tab * 4
chi <- chisq.test(tab, correct = FALSE) # p-value = 0.05683
expect_equal(ci_cramersv(chi, probs = c(0.05, 1))$interval[1], 0)
tab <- tab * 1.1
chi <- chisq.test(tab, correct = FALSE) # p-value = 0.04576
expect_equal(ci_cramersv(chi, probs = c(0.05, 1))$interval[1], 0.08590156, tolerance = 0.001)


test_that("ci_cramersv() gives consistent one- and two-sided CIs", {
  for (ci in c(ci_cramersv, ci_chisq_ncp)) {
    for (t in c("chi-squared", "bootstrap")) {
      out <- ci(
        X, type = t, boot_type = "norm", R = 99L, seed = 1L, probs = c(0.1, 0.8)
      )$interval

      outl <- ci(
        X, type = t, boot_type = "norm", R = 99L, seed = 1L, probs = c(0.1, 1)
      )$interval[1L]

      outr <- ci(
        X, type = t, boot_type = "norm", R = 99L, seed = 1L, probs = c(0, 0.8)
      )$interval[2L]

      expect_equal(out[1L], outl)
      expect_equal(out[2L], outr)
    }
  }
})

test_that("resulting object is complete", {
  comps <- c("parameter", "interval", "estimate", "probs", "type", "info")

  expect_equal(names(ci_cramersv(X)), comps)
  expect_equal(names(ci_cramersv(X, type = "boot", boot_type = "perc", R = 99L)), comps)
  expect_equal(names(ci_chisq_ncp(X)), comps)
  expect_equal(names(ci_chisq_ncp(X, type = "boot", boot_type = "perc", R = 99L)), comps)
})
