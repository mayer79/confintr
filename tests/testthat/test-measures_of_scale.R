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

test_that("ci_var() works", {
  expect_equal(ci_var(x)$estimate, stats::var(x))
  expect_equal(ci_var(x)$interval, res, tolerance = 1e-5)
  expect_no_error(ci_var(x, type = "bootstrap", boot_type = "perc", R = 99))
})

test_that("ci_sd() works", {
  expect_equal(ci_sd(x)$estimate, stats::sd(x))
  expect_equal(ci_sd(x)$interval, sqrt(res), tolerance = 1e-5)
  expect_no_error(ci_sd(x, type = "bootstrap", boot_type = "perc", R = 99))
})

test_that("ci_iqr() works", {
  expect_no_error(out <- ci_IQR(x, R = 449, boot_type = "perc"))
  expect_equal(out$estimate, stats::IQR(x))
})

test_that("ci_mad() works", {
  expect_no_error(out <- ci_mad(x, R = 449, boot_type = "perc"))
  expect_equal(out$estimate, stats::mad(x)  )
  expect_equal(
    ci_mad(x, R = 449, constant = 1, boot_type = "perc")$estimate,
    stats::mad(x, constant = 1)
  )
})
