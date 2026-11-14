make_data <- function() {
  set.seed(42)
  n <- 200
  d <- data.frame(pluie = rbinom(n, 1, 0.4), trafic = runif(n, 50, 150))
  d$y <- rpois(n, exp(-3 + 0.5 * d$pluie) * d$trafic)
  d
}

test_that("ajuster_modele_poisson ajuste un glm Poisson", {
  d <- make_data()
  m <- ajuster_modele_poisson(d, y ~ pluie)
  expect_s3_class(m, "glm")
  expect_equal(m$family$family, "poisson")
  expect_gt(unname(coef(m)["pluie"]), 0)
})

test_that("l'exposition est prise en compte via un offset", {
  d <- make_data()
  m <- ajuster_modele_poisson(d, y ~ pluie, exposition_col = "trafic")
  expect_equal(attr(m, "exposition_col"), "trafic")
  expect_equal(unname(coef(m)["(Intercept)"]), -3, tolerance = 0.3)
  d$trafic[1] <- 0
  expect_error(ajuster_modele_poisson(d, y ~ pluie, exposition_col = "trafic"),
               "strictement positive")
})

test_that("ajuster_modele_poisson valide ses entrees", {
  d <- make_data()
  expect_error(ajuster_modele_poisson(d, y ~ inconnue), "inconnue")
  expect_error(ajuster_modele_poisson(d, ~ pluie), "formule")
  d$y[1] <- -1
  expect_error(ajuster_modele_poisson(d, y ~ pluie), "negatives")
})

test_that("predire_accidents retourne des predictions coherentes", {
  d <- make_data()
  m <- ajuster_modele_poisson(d, y ~ pluie, exposition_col = "trafic")
  p <- predire_accidents(m, data.frame(pluie = c(0, 1), trafic = 100))
  expect_equal(nrow(p), 2)
  expect_gt(p$prediction[2], p$prediction[1])
  expect_true(all(p$ic_inf < p$prediction & p$prediction < p$ic_sup))
  expect_true(all(p$p_au_moins_un > 0 & p$p_au_moins_un < 1))
  expect_error(predire_accidents(m, data.frame(pluie = 0)), "trafic")
  expect_error(predire_accidents(lm(y ~ pluie, d), d), "glm")
})

test_that("diagnostiquer_surdispersion distingue Poisson et surdispersion", {
  set.seed(7)
  n <- 200
  d <- data.frame(pluie = rbinom(n, 1, 0.4))
  d$y_pois <- rpois(n, 5)
  d$y_sur <- rnbinom(n, mu = 5, size = 0.5)
  diag_p <- diagnostiquer_surdispersion(ajuster_modele_poisson(d, y_pois ~ pluie))
  diag_s <- diagnostiquer_surdispersion(ajuster_modele_poisson(d, y_sur ~ pluie))
  expect_gt(diag_p$dispersion, 0.7)
  expect_lt(diag_p$dispersion, 1.4)
  expect_gt(diag_s$dispersion, 3)
  expect_match(diag_s$conclusion, "Surdispersion")
  expect_output(print(diag_s), "Diagnostic")
})
