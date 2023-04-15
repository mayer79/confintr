x <- seq(0, pi / 1.5, by = 0.06)
y <- sin(x)
# plot(x, y)
n <- length(x)

test_that("title_case1() helper function works", {
  expect_equal(title_case1("hello"), "Hello")
})

test_that("Estimates are consistent", {
  for (m in c("p", "s", "k")) {
    expect_equal(
      ci_cor(x, y, method = m, type = "boot", boot_type = "perc", R = 99L)$estimate,
      stats::cor(x, y, method = m)
    )
  }
})

test_that("Missing values are dropped pairwise", {
  expect_equal(
    ci_cor(x[2:(n-1L)], y[2:(n-1L)]),
    ci_cor(c(NA, x[-1L]), c(y[-n], NA))
  )
})

test_that("Matrix/data.frame API is consistent with x, y API", {
  expect_error(ci_cor(iris))
  expect_error(ci_cor(iris[, 4:5]))
  expect_equal(ci_cor(iris[, 1:2]), ci_cor(iris[, 1L], iris[, 2L]))
})

test_that("Lengths of x and y must agree", {
  expect_error(ci_cor(1:10, 1:11))
})

test_that("Rank-correlations give error if not bootstrapped", {
  expect_no_error(ci_cor(x, y, method = "p", type = "normal"))
  expect_error(ci_cor(x, y, method = "k", type = "normal"))
  expect_error(ci_cor(x, y, method = "s", type = "normal"))
})

test_that("CIs for Pearson corrs equal those from stats::cor.test()", {
  expect_equal(ci_cor(x, y)$interval, c(stats::cor.test(x, y)$conf.int))
  expect_equal(
    ci_cor(x, y, probs = c(0.05, 0.95))$interval,
    c(stats::cor.test(x, y, conf.level = 0.9)$conf.int)
  )
  expect_equal(
    ci_cor(x, y, probs = c(0.05, 1))$interval,
    c(stats::cor.test(x, y, alternative = "greater")$conf.int)
  )
  expect_equal(
    ci_cor(x, y, probs = c(0, 0.9))$interval,
    c(stats::cor.test(x, y, alternative = "less", conf.level = 0.9)$conf.int)
  )
})

test_that("Bootstrapped CIs agree with online example", {
  # I could not find another source, would love to have a more official one

  # Data and slightly adapted answer from
  # https://stackoverflow.com/questions/58393608/bootstrapped-correlation-in-r
  X <- read.table(text = "
      x            y
1   .6080522    1.707642
2   1.4307273   1.772616
3   0.8226198   1.768537
4   1.7714221   1.265276
5   1.5986213   1.855719
6   1.0000000   1.606106
7   1.1678940   1.671457
8   0.6630012   1.608428
9   1.0842423   1.670619
10  0.5592512   1.107783
11  1.6442616   1.492832
12  0.8326965   1.643923
13  1.1696954   1.763181
14  0.7484543   1.762921
15  1.0842423   1.591566
16  0.9014748   1.718669
17  0.7604917   1.782863
18  0.8566499   1.796216
19  1.4307273   1.913675
20  1.7579695   1.903155")

  set.seed(1L)
  b3 <- boot::boot(
    X, statistic = function(data, i) {
      stats::cor(data[i, "x"], data[i, "y"], method = "pearson")
    }, R = 1000
  )
  boot_types <- c("norm", "basic", "perc", "bca")
  boot_out <- boot::boot.ci(b3, type = boot_types)

  for (t in boot_types) {
    confintr_out <- ci_cor(X, R = 1000L, seed = 1L, type = "boot", boot_type = t)
    expect_equal(
      confintr_out$interval,
      unname(utils::tail(boot_out[[map_boot_type(t)]][1L, ], 2L))
    )
  }
})

test_that("ci_cor() gives consistent one- and two-sided CIs", {
  expect_error(ci_cor(x, y, probs = c(0.1, 0.8)))

  for (t in c("normal", "bootstrap")) {
    out <- ci_cor(
      x, y, type = t, boot_type = "perc", R = 99L, seed = 1L, probs = c(0.1, 0.9)
    )$interval

    outl <- ci_cor(
      x, y, type = t, boot_type = "perc", R = 99L, seed = 1L, probs = c(0.1, 1)
    )$interval[1L]

    outr <- ci_cor(
      x, y, type = t, boot_type = "perc", R = 99L, seed = 1L, probs = c(0, 0.9)
    )$interval[2L]

    expect_equal(out[1L], outl)
    expect_equal(out[2L], outr)
  }

  for (m in c("s", "k")) {
    out <- ci_cor(
      x, y, method = m, type = "boot", boot_type = "perc",
      R = 99L, seed = 1L, probs = c(0.1, 0.8)
    )$interval

    outl <- ci_cor(
      x, y, method = m, type = "boot", boot_type = "perc",
      R = 99L, seed = 1L, probs = c(0.1, 1)
    )$interval[1L]

    outr <- ci_cor(
      x, y, method = m, type = "boot", boot_type = "perc",
      R = 99L, seed = 1L, probs = c(0, 0.8)
    )$interval[2L]

    expect_equal(out[1L], outl)
    expect_equal(out[2L], outr)
  }
})

test_that("resulting object is complete", {
  comps <- c("parameter", "interval", "estimate", "probs", "type", "info")

  expect_equal(names(ci_cor(x, y)), comps)
  expect_equal(names(ci_cor(x, y, type = "boot", boot_type = "perc", R = 99L)), comps)
})
