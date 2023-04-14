p <- c(0.1, 0.9)

in_nms <- c("norm","basic", "stud", "perc", "bca")  # input to boot::boot.ci()
out_nms <- c("normal", "basic", "student", "percent", "bca")  # Output to boot::boot.ci()

set.seed(1L)
S <- boot::boot(1:100, statistic = function(x, id) mean(x[id]), R = 199L)

# Same with variance
set.seed(1L)
S_t <- boot::boot(
  1:100, statistic = function(x, id) c(mean(x[id]), se_mean(x[id])^2), R = 199L
)

test_that("ci_boot() gives identical results if variance is passed", {
  for (t in in_nms[-3L]) {
    expect_equal(
      ci_boot(S, boot_type = t, probs = p), ci_boot(S_t, boot_type = t, probs = p)
    )
  }
  expect_warning(ci_boot(S, boot_type = in_nms[3L], probs = p))
  expect_no_warning(ci_boot(S_t, boot_type = in_nms[3L], probs = p))
})

test_that("ci_boot() is deterministic given S", {
  for (t in in_nms) {
    expect_equal(
      ci_boot(S_t, boot_type = t, probs = p),
      ci_boot(S_t, boot_type = t, probs = p)
    )
  }
})

test_that("ci_boot() is monotonic in the probs", {
  p1 <- c(0.05, 0.95)

  for (t in in_nms) {
    out_p <- ci_boot(S_t, boot_type = t, probs = p)
    out_p1 <- ci_boot(S_t, boot_type = t, probs = p1)

    expect_true(out_p[1L] >= out_p1[1L])
    expect_true(out_p[2L] <= out_p1[2L])
  }
})

test_that("ci_boot() is consistent in the unequal-tailed case", {
  p1 <- c(0.1, 0.95)
  p2 <- c(0.05, 0.9)

  for (t in in_nms) {
    out_p <- ci_boot(S_t, boot_type = t, probs = p)
    out_p1 <- ci_boot(S_t, boot_type = t, probs = p1)
    out_p2 <- ci_boot(S_t, boot_type = t, probs = p2)

    expect_equal(out_p[1L], out_p1[1L])
    expect_true(out_p[2L] <= out_p1[2L])

    expect_true(out_p[1L] >= out_p2[1L])
    expect_equal(out_p[2L], out_p2[2L])
  }
})

test_that("ci_boot() is consistent in the one-tailed situation", {
  p1 <- c(0.1, 1)
  p2 <- c(0, 0.9)

  for (t in in_nms) {
    out_p <- ci_boot(S_t, boot_type = t, probs = p)
    out_p1 <- ci_boot(S_t, boot_type = t, probs = p1)
    out_p2 <- ci_boot(S_t, boot_type = t, probs = p2)

    expect_equal(out_p[1L], out_p1[1L])
    expect_equal(unname(out_p1[2L]), Inf)

    expect_equal(out_p[2L], out_p2[2L])
    expect_equal(unname(out_p2[1L]), -Inf)
  }
})

test_that("ci_boot() replicates specific example in ?boot:boot.ci", {
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

  # Now with ci_boot()
  set.seed(1L)
  S_t <- boot::boot(
    aircondit$hours,
    statistic = function(x, id) c(mean(x[id]), se_mean(x[id])^2),
    R = 999L
  )

  for (t in in_nms) {
    expect_equal(
      ci_boot(S_t, t, c(0.025, 0.975)),
      unname(utils::tail(boot_out[[map_boot_type(t)]][1L, ], 2L))
    )
  }
  # ci_mean(aircondit, type = "bootstrap", boot_type = "norm", R = 999L, seed = 1L)
})

test_that("check_bca() works", {
  expect_error(check_bca("bca", n = 1000, R = 99L))
  expect_no_error(check_bca("perc", n = 1000, R = 99L))
  expect_no_error(check_bca("perc", n = 10, R = 99L))
})

test_that("map_boot_type() works in line with in- and output of boot::boot.ci()", {
  expect_error(map_boot_type("no_valid"))

  for (i in seq_along(in_nms)) {
    expect_equal(map_boot_type(in_nms[i]), out_nms[i])
  }
})

test_that("boot_info() makes sense", {
  expect_equal(boot_info("some_parametric_method", boot_type = "bca", R = 99L), NULL)

  expect_equal(
    boot_info("bootstrap", boot_type = "bca", R = 99L),
    "based on 99 bootstrap replications and the bca method"
  )
  expect_false(
    boot_info("bootstrap", boot_type = "bca", R = 99L) ==
      boot_info("bootstrap", boot_type = "perc", R = 99L),
  )
  expect_false(
    boot_info("bootstrap", boot_type = "bca", R = 99L) ==
      boot_info("bootstrap", boot_type = "bca", R = 999L),
  )
})
