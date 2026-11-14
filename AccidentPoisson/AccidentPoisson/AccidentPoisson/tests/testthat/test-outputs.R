test_that("generer_alerte attribue les niveaux", {
  z <- data.frame(zone = c("A", "B", "C"), total = c(5, 12, 20))
  r <- generer_alerte(z, "total", seuil = 10)
  expect_equal(r$niveau_vigilance,
               c("Surveillance habituelle", "Vigilance renforcee", "Vigilance renforcee"))
  r2 <- generer_alerte(z, "total", seuil = 10, seuil_critique = 15)
  expect_equal(r2$niveau_vigilance[3], "Alerte critique")
  expect_true(all(c("niveau_vigilance", "recommandation") %in% names(r2)))
  expect_error(generer_alerte(z, "total", seuil = 10, seuil_critique = 5))
  expect_error(generer_alerte(z, "absent", seuil = 10), "introuvable")
})

test_that("generer_alerte gere les valeurs manquantes", {
  z <- data.frame(total = c(NA, 12))
  r <- generer_alerte(z, "total", seuil = 10)
  expect_equal(r$niveau_vigilance[1], "Donnee manquante")
})

test_that("calculer_seuil_alerte est coherent avec Poisson", {
  s <- calculer_seuil_alerte(3, 0.95)
  expect_equal(s, 7)
  expect_lte(ppois(s - 1, 3, lower.tail = FALSE), 0.05)
})

test_that("visualiser_comptages retourne un ggplot", {
  skip_if_not_installed("ggplot2")
  z <- data.frame(zone = c("A", "B", "C"), total_accidents = c(12, 30, 7))
  expect_s3_class(visualiser_comptages(z, "zone", "total_accidents"), "ggplot")
  expect_s3_class(
    visualiser_comptages(z, "zone", "total_accidents", type = "points", horizontal = TRUE),
    "ggplot")
  expect_error(visualiser_comptages(z, "zone", "absent"), "introuvable")
  expect_error(visualiser_comptages(z, "total_accidents", "zone"), "numerique")
})

test_that("exporter_resultats ecrit csv, rds et xlsx", {
  d <- data.frame(a = 1:3, b = c("x", "y", "z"))
  f_csv <- tempfile(fileext = ".csv")
  exporter_resultats(d, f_csv)
  expect_equal(nrow(read.csv(f_csv)), 3)
  expect_error(exporter_resultats(d, f_csv, ecraser = FALSE), "existe")
  f_rds <- tempfile(fileext = ".rds")
  exporter_resultats(d, f_rds)
  expect_equal(readRDS(f_rds), d)
  expect_error(exporter_resultats(d, tempfile(fileext = ".txt")), "Format")
  skip_if_not_installed("writexl")
  f_x <- tempfile(fileext = ".xlsx")
  exporter_resultats(list(un = d, deux = d), f_x)
  expect_true(file.exists(f_x))
})

test_that("charger_exemple_rn3 charge le jeu de demonstration", {
  d <- charger_exemple_rn3()
  expect_equal(nrow(d), 1096)
  expect_s3_class(d$Date, "Date")
  expect_true("Nb_Accidents" %in% names(d))
})
