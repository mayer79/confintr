x <- exp(seq(0, 1, by = 0.01))

test_that("estimates agree with usual estimates", {
  expect_equal(ci_mean(x)$estimate, mean(x))
  expect_equal(
    ci_quantile(x, q = 0.4)$estimate,
    stats::quantile(x, probs = 0.4, names = FALSE)
  )
  expect_equal(ci_median(x)$estimate, stats::median(x))
})

test_that("ci_mean() (Student method) gives same as stats::t.test()", {
  out <- ci_mean(x)

  expect_equal(out$interval, c(stats::t.test(x)$conf.int))
  expect_equal(
    ci_mean(x, probs = c(0.05, 0.95))$interval,
    c(stats::t.test(x, conf.level = 0.9)$conf.int)
  )
  expect_equal(
    ci_mean(x, probs = c(0.05, 1))$interval,
    c(stats::t.test(x, alternative = "greater")$conf.int)
  )
  expect_equal(
    ci_mean(x, probs = c(0, 0.9))$interval,
    c(stats::t.test(x, alternative = "less", conf.level = 0.9)$conf.int)
  )
})

test_that("ci_mean() gives error when data is constant", {
  bad_x <- rep(1, 100L)
  expect_error(ci_mean(bad_x))
  expect_error(ci_mean(bad_x, type = "Wald"))
})

test_that("Wald CI agree with classic formula", {
  out <- ci_mean(x, type = "Wald")
  expect_equal(
    out$interval,
    mean(x) + c(-1, 1) * stats::sd(x) / sqrt(length(x)) * stats::qnorm(0.975)
  )
})

test_that("ci_mean() gives consistent one- and two-sided intervals for all types", {
  for (t in c("t", "Wald", "bootstrap")) {
    out <- ci_mean(x, type = t, R = 99L, seed = 1L, probs = c(0.1, 0.8))$interval
    outl <- ci_mean(x, type = t, R = 99L, seed = 1L, probs = c(0.1, 1))$interval[1L]
    outr <- ci_mean(x, type = t, R = 99L, seed = 1L, probs = c(0, 0.8))$interval[2L]

    expect_equal(out[1L], outl)
    expect_equal(out[2L], outr)
  }
})

test_that("Bootstrap CIs (all types) correspond with example in boot::boot.ci()", {
  in_nms <- c("norm","basic", "stud", "perc", "bca")
  data("aircondit", package = "boot")
  # Copied with slight modification from ?boot::boot.ci help
  mean.fun <- function(d, i) {
    m <- mean(d$hours[i])
    n <- length(i)
    v <- (n - 1) * stats::var(d$hours[i]) / n^2
    c(m, v)
  }
  set.seed(1L)
  air.boot <- boot::boot(aircondit, mean.fun, R = 999L)
  boot_out <- boot::boot.ci(air.boot, type = in_nms)

  for (t in in_nms) {
    confintr_out <- ci_mean(
      aircondit, type = "bootstrap", boot_type = t, R = 999L, seed = 1L
    )
    expect_equal(
      confintr_out$interval,
      unname(utils::tail(boot_out[[map_boot_type(t)]][1L, ], 2L))
    )
  }
})

test_that("ci_quantile (non-parametric) is consistent with other implementations", {
  expect_equal(
    ci_quantile(x, q = 0.4)$interval,
    c(1.349859, 1.648721),  # jmuOutlier::quantileCI(x, probs = 0.4, conf.level = 0.95)
    tolerance = 1e-5
  )
  expect_equal(
    ci_quantile(x, q = 0.5, probs = c(0.05, 0.95))$interval,
    c(1.506818, 1.803988),  # jmuOutlier::quantileCI(x, probs = 0.5, conf.level = 0.9)
    tolerance = 1e-5
  )
})

test_that("ci_quantile() gives consistent one- and two-sided intervals for all types", {
  for (t in c("binomial", "bootstrap")) {
    out <- ci_quantile(
      x, q = 0.4, type = t, boot_type = "perc", R = 99L, seed = 1L, probs = c(0.1, 0.8)
    )$interval

    outl <- ci_quantile(
      x, q = 0.4, type = t, boot_type = "perc", R = 99L, seed = 1L, probs = c(0.1, 1)
    )$interval[1L]

    outr <- ci_quantile(
      x, q = 0.4, type = t, boot_type = "perc", R = 99L, seed = 1L, probs = c(0, 0.8)
    )$interval[2L]

    expect_equal(out[1L], outl)
    expect_equal(out[2L], outr)
  }
})

test_that("ci_median() is consistent with ci_quantile()", {
  expect_equal(ci_median(x)$interval, ci_quantile(x, q = 0.5)$interval)
  expect_equal(
    ci_median(x, type = "bootstrap", boot_type = "perc", seed = 1L, R = 99L)$interval,
    ci_quantile(
      x, type = "bootstrap", boot_type = "perc", seed = 1L, R = 99L, q = 0.5
    )$interval
  )
})

test_that("resulting object is complete", {
  comps <- c("parameter", "interval", "estimate", "probs", "type", "info")

  expect_equal(names(ci_mean(x)), comps)
  expect_equal(names(ci_mean(x, type = "Wald")), comps)
  expect_equal(names(ci_mean(x, type = "boot", boot_type = "perc", R = 99L)), comps)

  expect_equal(names(ci_mean(x, type = "boot", boot_type = "perc", R = 99L)), comps)

  expect_equal(names(ci_median(x)), comps)
  expect_equal(names(ci_median(x, type = "boot", boot_type = "perc", R = 99L)), comps)

  expect_equal(names(ci_quantile(x, q = 0.4)), comps)
  expect_equal(
    names(ci_quantile(x, q = 0.4, type = "boot", boot_type = "perc", R = 99L)), comps
  )
})
