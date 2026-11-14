test_that("analyser_zones classe correctement les zones", {
  d <- data.frame(zone = rep(c("A", "B", "C"), each = 100),
                  n = c(rep(1, 100), rep(1, 100), rep(5, 100)))
  res <- analyser_zones(d, "zone", count_col = "n")
  expect_equal(res$zone[1], "C")
  expect_equal(res$rang[1], 1)
  expect_equal(res$classification[res$zone == "C"], "Risque eleve")
  expect_equal(res$classification[res$zone == "A"], "Risque faible")
  expect_equal(sum(res$part_accidents), 1)
  expect_equal(res$score_risque[1], 10)
})

test_that("zones equivalentes : dans la moyenne", {
  d <- data.frame(zone = rep(c("A", "B"), each = 50), n = 3)
  res <- analyser_zones(d, "zone", count_col = "n")
  expect_true(all(res$classification == "Dans la moyenne"))
})

test_that("donnees d'evenements et noms de colonnes libres", {
  d <- data.frame(ville = c(rep("X", 30), rep("Y", 10)))
  res <- analyser_zones(d, "ville")
  expect_equal(res$total_accidents[res$zone == "X"], 30)
  expect_equal(res$part_accidents[res$zone == "X"], 0.75)
  d2 <- data.frame(date_obs = 1:4, ville = c("a", "a", "b", "b"), nombre = c(1, 2, 3, 4))
  res2 <- analyser_zones(d2, "ville", "nombre")
  expect_equal(res2$total_accidents[res2$zone == "b"], 7)
})

test_that("exposition prise en compte", {
  d <- data.frame(z = c("A", "B"), n = c(10, 10), km = c(10, 100))
  res <- analyser_zones(d, "z", "n", exposition_col = "km")
  expect_equal(res$taux[res$zone == "A"], 1)
  expect_equal(res$rang[res$zone == "A"], 1)
})

test_that("analyser_zones valide ses entrees", {
  d <- data.frame(z = c("A", "B"), n = c(1, 2))
  expect_error(analyser_zones(d, "absent"), "introuvable")
  expect_error(analyser_zones(d, "z", "n", niveau = 2))
  expect_error(analyser_zones(data.frame(z = "A", n = 0), "z", "n"), "Aucun accident")
})
