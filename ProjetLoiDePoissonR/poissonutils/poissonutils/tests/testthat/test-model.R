make_data <- function() {
  set.seed(42)
  n <- 200
  d <- data.frame(rain = rbinom(n, 1, 0.4), traffic = runif(n, 50, 150))
  d$y <- rpois(n, exp(-3 + 0.5 * d$rain) * d$traffic)
  d
}

test_that("fit_poisson_model fits a Poisson glm", {
  d <- make_data()
  m <- fit_poisson_model(d, y ~ rain)
  expect_s3_class(m, "glm")
  expect_equal(m$family$family, "poisson")
  expect_gt(unname(coef(m)["rain"]), 0)
})

test_that("exposure is handled through an offset", {
  d <- make_data()
  m <- fit_poisson_model(d, y ~ rain, exposure_col = "traffic")
  expect_equal(attr(m, "exposure_col"), "traffic")
  expect_equal(unname(coef(m)["(Intercept)"]), -3, tolerance = 0.3)
  d$traffic[1] <- 0
  expect_error(fit_poisson_model(d, y ~ rain, exposure_col = "traffic"),
               "strictly positive")
})

test_that("fit_poisson_model validates its inputs", {
  d <- make_data()
  expect_error(fit_poisson_model(d, y ~ unknown), "unknown")
  expect_error(fit_poisson_model(d, ~ rain), "formula")
  d$y[1] <- -1
  expect_error(fit_poisson_model(d, y ~ rain), "negative")
})

test_that("predict_counts returns coherent predictions", {
  d <- make_data()
  m <- fit_poisson_model(d, y ~ rain, exposure_col = "traffic")
  p <- predict_counts(m, data.frame(rain = c(0, 1), traffic = 100))
  expect_equal(nrow(p), 2)
  expect_gt(p$prediction[2], p$prediction[1])
  expect_true(all(p$ci_lower < p$prediction & p$prediction < p$ci_upper))
  expect_true(all(p$prob_at_least_one > 0 & p$prob_at_least_one < 1))
  expect_error(predict_counts(m, data.frame(rain = 0)), "traffic")
  expect_error(predict_counts(lm(y ~ rain, d), d), "glm")
})

test_that("check_overdispersion separates Poisson from overdispersed data", {
  set.seed(7)
  n <- 200
  d <- data.frame(rain = rbinom(n, 1, 0.4))
  d$y_pois <- rpois(n, 5)
  d$y_over <- rnbinom(n, mu = 5, size = 0.5)
  chk_p <- check_overdispersion(fit_poisson_model(d, y_pois ~ rain))
  chk_o <- check_overdispersion(fit_poisson_model(d, y_over ~ rain))
  expect_gt(chk_p$dispersion, 0.7)
  expect_lt(chk_p$dispersion, 1.4)
  expect_gt(chk_o$dispersion, 3)
  expect_match(chk_o$conclusion, "Overdispersion")
  expect_output(print(chk_o), "Overdispersion")
})
