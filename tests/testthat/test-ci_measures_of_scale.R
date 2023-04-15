# Example for variance and std: https://faculty.elgin.edu/dkernler/statistics/ch09/9-3.html
x <- unlist(
  read.table(
    text =
      "61	63	64	65	65
        67	71	72	73	74
        75	77	79	80	81
        82	83	83	84	85
        86	86	89	95	95"
  ),
  use.names = FALSE
)
res <- c(58.12406, 184.49901)  # See weblink above

test_that("CIs give consistent estimates", {
  expect_equal(ci_var(x, boot_type = "perc", R = 99L)$estimate, stats::var(x))
  expect_equal(ci_sd(x, boot_type = "perc", R = 99L)$estimate, stats::sd(x))
  expect_equal(ci_IQR(x, boot_type = "perc", R = 99L)$estimate, stats::IQR(x))
  expect_equal(ci_mad(x, boot_type = "perc", R = 99L)$estimate, stats::mad(x))
})

test_that("ci_var() gives identical result as web example", {
  expect_equal(ci_var(x)$interval, res, tolerance = 1e-5)
})

test_that("ci_var() (classic, bootstrap) is consistent with DescTools::VarCI()", {
  # DescTools version: ‘0.99.48’
  # DescTools::VarCI(x)
  expect_equal(ci_var(x)$interval, c(58.12406, 184.49902), tolerance = 1e-5)

  # set.seed(1L); DescTools::VarCI(x, method = "bca", R = 99L)
  expect_equal(
    ci_var(x, type = "boot", R = 99L, seed = 1L, boot_type = "bca")$interval,
    c(60.84690, 141.02359),
    tolerance = 1e-5
  )
})

test_that("ci_IQR/mad/var() gives consistent one- and two-sided intervals", {
  for (ci in c(ci_IQR, ci_mad, ci_var)) {
    out <- ci(x, boot_type = "perc", R = 99L, seed = 1L, probs = c(0.1, 0.8))$interval
    outl <- ci(x, boot_type = "perc", R = 99L, seed = 1L, probs = c(0.1, 1))$interval[1L]
    outr <- ci(x, boot_type = "perc", R = 99L, seed = 1L, probs = c(0, 0.8))$interval[2L]

    expect_equal(out[1L], outl)
    expect_equal(out[2L], outr)
  }
})

test_that("ci_sd() is consistent with ci_var()", {
  expect_equal(ci_sd(x)$interval^2, ci_var(x)$interval)
  expect_equal(
    ci_sd(x, type = "bootstrap", boot_type = "perc", seed = 1L, R = 99L)$interval^2,
    ci_var(x, type = "bootstrap", boot_type = "perc", seed = 1L, R = 99L)$interval
  )
})

test_that("resulting object is complete", {
  comps <- c("parameter", "interval", "estimate", "probs", "type", "info")
  expect_equal(names(ci_var(x)), comps)
  expect_equal(names(ci_sd(x)), comps)
  expect_equal(names(ci_IQR(x, boot_type = "perc", R = 99L)), comps)
  expect_equal(names(ci_mad(x, boot_type = "perc", R = 99L)), comps)
})
