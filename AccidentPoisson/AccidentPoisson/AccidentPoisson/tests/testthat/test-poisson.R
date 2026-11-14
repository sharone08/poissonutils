test_that("calculer_poisson reproduit dpois/ppois", {
  expect_equal(calculer_poisson(3, 2), exp(-3) * 3^2 / 2)
  expect_equal(calculer_poisson(3, 2, cumul = TRUE), sum(dpois(0:2, 3)))
  expect_equal(calculer_poisson(3, 2, superieur = TRUE), 1 - sum(dpois(0:2, 3)))
})

test_that("calculer_poisson valide ses arguments", {
  expect_error(calculer_poisson(-1, 2), "lambda")
  expect_error(calculer_poisson(NA, 2), "lambda")
  expect_error(calculer_poisson(3, 1.5), "entiers")
  expect_error(calculer_poisson(3, -1), "entiers")
  expect_error(calculer_poisson(3, 2, cumul = TRUE, superieur = TRUE))
})

test_that("table_poisson est coherent", {
  res <- table_poisson(3, 10, jours = 365)
  expect_equal(nrow(res), 11)
  expect_equal(res$p_cumulee + res$p_superieure, rep(1, 11))
  expect_equal(res$jours_attendus, res$p_egal * 365)
  expect_error(table_poisson(3, -1))
  expect_error(table_poisson(c(1, 2), 5))
})

test_that("estimer_lambda donne la moyenne et un IC qui la contient", {
  res <- estimer_lambda(c(2, 3, 4, 3))
  expect_equal(res$lambda, 3)
  expect_lt(res$ic_inf, 3)
  expect_gt(res$ic_sup, 3)
  expect_equal(estimer_lambda(c(1, NA, 3))$n, 2)
  expect_error(estimer_lambda(c(1.5, 2)))
})

test_that("tester_adequation_poisson accepte un ajustement parfait", {
  x <- rep(0:8, times = round(1000 * dpois(0:8, 3)))
  res <- tester_adequation_poisson(x)
  expect_s3_class(res, "test_poisson")
  expect_gt(res$p_value, 0.5)
  expect_output(print(res), "Poisson")
})

test_that("tester_adequation_poisson rejette des donnees bimodales", {
  res <- tester_adequation_poisson(c(rep(0, 500), rep(10, 500)))
  expect_lt(res$p_value, 0.001)
})
