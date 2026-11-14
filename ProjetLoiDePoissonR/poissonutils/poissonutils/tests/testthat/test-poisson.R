test_that("poisson_prob matches the Poisson formula", {
  expect_equal(poisson_prob(2, lambda = 3), exp(-3) * 3^2 / 2)
  expect_equal(poisson_prob(2, lambda = 3, type = "cumulative"), sum(dpois(0:2, 3)))
  expect_equal(poisson_prob(2, lambda = 3, type = "upper"), 1 - sum(dpois(0:2, 3)))
  expect_equal(poisson_prob(0:3, lambda = 2), dpois(0:3, 2))
})

test_that("poisson_prob validates its arguments", {
  expect_error(poisson_prob(2, lambda = -1), "lambda")
  expect_error(poisson_prob(2, lambda = NA), "lambda")
  expect_error(poisson_prob(1.5, lambda = 3), "whole numbers")
  expect_error(poisson_prob(-1, lambda = 3), "whole numbers")
  expect_error(poisson_prob("a", lambda = 3), "whole numbers")
  expect_error(poisson_prob(2, lambda = 3, type = "other"))
})

test_that("poisson_table is consistent", {
  res <- poisson_table(3, 10, days = 365)
  expect_equal(nrow(res), 11)
  expect_equal(res$cum_prob + res$upper_prob, rep(1, 11))
  expect_equal(res$expected_days, res$prob * 365)
  expect_error(poisson_table(3, -1))
  expect_error(poisson_table(c(1, 2), 5))
})

test_that("estimate_lambda gives the mean and a CI containing it", {
  res <- estimate_lambda(c(2, 3, 4, 3))
  expect_equal(res$lambda, 3)
  expect_lt(res$ci_lower, 3)
  expect_gt(res$ci_upper, 3)
  expect_equal(estimate_lambda(c(1, NA, 3))$n, 2)
  expect_error(estimate_lambda(c(1.5, 2)))
})

test_that("poisson_gof_test accepts a perfect Poisson fit", {
  x <- rep(0:8, times = round(1000 * dpois(0:8, 3)))
  res <- poisson_gof_test(x)
  expect_s3_class(res, "poisson_gof")
  expect_gt(res$p_value, 0.5)
  expect_output(print(res), "Poisson")
})

test_that("poisson_gof_test rejects bimodal data", {
  res <- poisson_gof_test(c(rep(0, 500), rep(10, 500)))
  expect_lt(res$p_value, 0.001)
})
