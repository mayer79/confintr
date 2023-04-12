test_that("ci_cor works", {
  set.seed(1)
  x <- 1:24
  y <- x * 0.2 - runif(length(x))
  expect_equal(ci_cor(x, y)$estimate, cor(x, y))
  expect_equal(ci_cor(x, y, method = "k", R = 29, type = "bootstrap",
                      boot_type = "norm")$estimate, cor(x, y, method = "k"))
  expect_equal(ci_cor(x, y, method = "s", R = 29, type = "bootstrap",
                      boot_type = "norm")$estimate, cor(x, y, method = "s"))
  expect_equal(ci_cor(x, y)$interval, as.numeric(cor.test(x, y)$conf.int))

  expect_equal(ci_cor(x, y, type = "bootstrap", R = 499, seed = 1)$interval,
               c(0.9571956, 0.9868392), tolerance = 0.001)
  expect_equal(ci_cor(x, y, type = "bootstrap", R = 499, seed = 1, method = "s",
                      boot_type = "norm")$interval, c(0.9530945, 1.0000000), tolerance = 0.001)
  expect_equal(ci_cor(x, y, type = "bootstrap", R = 499, seed = 1, method = "k",
                      boot_type = "norm")$interval, c(0.8028851, 0.9592290), tolerance = 0.001)
  expect_equal(ci_cor(x, y, type = "bootstrap", R = 499, seed = 1, method = "k", probs = c(0.05, 1),
                      boot_type = "norm")$interval, c(0.8154531, 1.0000000), tolerance = 0.001)
  expect_equal(ci_cor(x, y, type = "bootstrap", R = 499, seed = 1, method = "k", probs = c(00, 0.95),
                      boot_type = "norm")$interval, c(-1.000000, 0.946661), tolerance = 0.001)
  expect_equal(ci_cor(x, y)$interval, c(0.9504543, 0.9908605), tolerance = 0.001)
  expect_equal(ci_cor(x, y)$interval[2], ci_cor(x, y, probs = c(0, 0.975))$interval[2])
  expect_equal(ci_cor(x, y)$interval[1], ci_cor(x, y, probs = c(0.025, 1))$interval[1])
  expect_equal(ci_cor(x, y)$interval[1], ci_cor(x, y, probs = c(0.025, 1))$interval[1])
  expect_equal(ci_cor(x, y, probs = c(0, 0.95))$interval[2],
               as.numeric(cor.test(x, y, conf.level = 0.95, alternative = "less")$conf.int[2]))
  expect_equal(ci_cor(x, y, probs = c(0.05, 1))$interval[1],
               as.numeric(cor.test(x, y, conf.level = 0.95, alternative = "greater")$conf.int[1]))
})

