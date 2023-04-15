x <- exp(seq(0, 1, by = 0.05))
y <- seq(0, 1, by = 0.3)

test_that("estimates agree with usual estimates", {
  expect_equal(ci_mean_diff(x, y)$estimate, mean(x) - mean(y))
  expect_equal(
    ci_median_diff(x, y, R = 199L)$estimate,
    stats::median(x) - stats::median(y)
  )
  expect_equal(
    ci_quantile_diff(x, y, q = 0.7, R = 199L)$estimate,
    stats::quantile(x, probs = 0.7, names = FALSE) -
      stats::quantile(y, probs = 0.7, names = FALSE)
  )
})

test_that("ci_mean_diff() (Student method) gives same as stats::t.test()", {
  for (v in c(TRUE, FALSE)) {
    expect_equal(
      ci_mean_diff(x, y, var.equal = v)$interval,
      c(stats::t.test(x, y, var.equal = v)$conf.int)
    )
    expect_equal(
      ci_mean_diff(x, y, probs = c(0.05, 0.95), var.equal = v)$interval,
      c(stats::t.test(x, y, conf.level = 0.9, var.equal = v)$conf.int)
    )
    expect_equal(
      ci_mean_diff(x, y, probs = c(0.05, 1), var.equal = v)$interval,
      c(stats::t.test(x, y, alternative = "greater", var.equal = v)$conf.int)
    )
    expect_equal(
      ci_mean_diff(x, y, probs = c(0, 0.9), var.equal = v)$interval,
      c(
        stats::t.test(x, y, alternative = "less", conf.level = 0.9, var.equal = v
        )$conf.int)
    )
  }
})

test_that("CIs remove missing values", {
  expect_equal(
    ci_mean_diff(x, y),
    ci_mean_diff(c(NA, x), c(y, NA, NA))
  )
  expect_equal(
    ci_quantile_diff(x, y, q = 0.6, R = 199L, seed = 1L),
    ci_quantile_diff(c(NA, x), c(y, NA, NA), q = 0.6, R = 199L, seed = 1L)
  )
})

test_that("CIs give error when vectors have length 0", {
  expect_error(ci_mean_diff(NA, y))
  expect_error(ci_mean_diff(x, y = NA))
  expect_error(ci_quantile_diff(NA, y, q = 0.6))
  expect_error(ci_quantile_diff(x, y = NA, q = 0.6))
})

test_that("Bootstrap CI for the mean difference agrees with DescTools::MeanDiffCI()", {
  # set.seed(1L); DescTools::MeanDiffCI(x, y, method = "bca", R = 199L)
  # DescTools version: ‘0.99.48’
  expect_equal(
    ci_mean_diff(x, y, type = "boot", R = 199L, seed = 1L, boot_type = "bca")$interval,
    c(0.8650208, 1.6263066),
    tolerance = 1e-5
  )

  # Cannot test method = "stud" (DescTools returns error)
})

test_that("Bootstrapped mean diffs correspond to example in boot::boot.ci()", {
  in_nms <- c("norm","basic", "stud", "perc", "bca")
  data(gravity, package = "boot")

  # Copy-pasted (with small formatting changes) from Example in ?boot::boot.ci
  diff.means <- function(d, f) {
    n <- nrow(d)
    gp1 <- 1:table(as.numeric(d$series))[1]
    m1 <- sum(d[gp1,1] * f[gp1])/sum(f[gp1])
    m2 <- sum(d[-gp1,1] * f[-gp1])/sum(f[-gp1])
    ss1 <- sum(d[gp1,1]^2 * f[gp1]) - (m1 *  m1 * sum(f[gp1]))
    ss2 <- sum(d[-gp1,1]^2 * f[-gp1]) - (m2 *  m2 * sum(f[-gp1]))
    c(m1 - m2, (ss1 + ss2)/(sum(f) - 2))
  }
  grav1 <- gravity[as.numeric(gravity[, 2]) >= 7, ]
  set.seed(1)
  grav1.boot <- boot::boot(
    grav1, diff.means, R = 999L, stype = "f", strata = grav1[ , 2]
  )
  boot_out <- boot::boot.ci(grav1.boot, type = in_nms)
  # End of example

  xx <- gravity[gravity$series == 7, 1L]
  yy <- gravity[gravity$series == 8, 1L]

  for (t in in_nms) {
    confintr_out <- ci_mean_diff(
      xx, yy, type = "bootstrap", boot_type = t, R = 999L, seed = 1L
    )
    expect_equal(
      confintr_out$interval,
      unname(utils::tail(boot_out[[map_boot_type(t)]][1L, ], 2L))
    )
  }
})

test_that("ci_quantile_diff() is correct via keeping one vector constant", {
  # TODO: Should have a better reference, but could not find one
  yy <- rep(3, times = 9)
  expect_equal(
    ci_quantile_diff(x, yy, q = 0.6, boot_type = "bca", R = 199L, seed = 1L)$interval,
    ci_quantile(
      x, q = 0.6, type = "boot", boot_type = "bca", R = 199L, seed = 1L
    )$interval - 3
  )
})

test_that("ci_quantile_diff() gives consistent one- and two-sided CIs", {
  out <- ci_quantile_diff(
    x, y, q = 0.6, boot_type = "perc", R = 99L, seed = 1L, probs = c(0.1, 0.8)
  )$interval

  outl <- ci_quantile_diff(
    x, y, q = 0.6, boot_type = "perc", R = 99L, seed = 1L, probs = c(0.1, 1)
  )$interval[1L]

  outr <- ci_quantile_diff(
    x, y, q = 0.6, boot_type = "perc", R = 99L, seed = 1L, probs = c(0, 0.8)
  )$interval[2L]

  expect_equal(out[1L], outl)
  expect_equal(out[2L], outr)
})

test_that("ci_median_diff() is consistent with ci_quantile_diff()", {
  expect_equal(
    ci_median_diff(
      x, y, type = "bootstrap", boot_type = "perc", seed = 1L, R = 99L
    )$interval,
    ci_quantile_diff(
      x, y, type = "bootstrap", boot_type = "perc", seed = 1L, R = 99L, q = 0.5
    )$interval
  )
})

test_that("resulting object is complete", {
  comps <- c("parameter", "interval", "estimate", "probs", "type", "info")

  expect_equal(names(ci_mean_diff(x, y)), comps)
  expect_equal(
    names(ci_mean_diff(x, y, type = "boot", boot_type = "perc", R = 99L)),
    comps
  )
  expect_equal(
    names(ci_mean_diff(x, y, type = "boot", boot_type = "perc", R = 99L)),
    comps
  )

  expect_equal(
    names(ci_median_diff(x, y, type = "boot", boot_type = "perc", R = 99L)),
    comps
  )
  expect_equal(
    names(ci_quantile_diff(x, y, q = 0.6, type = "boot", boot_type = "perc", R = 99L)),
    comps
  )
})
