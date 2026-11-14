ev <- data.frame(
  date = c("2025-01-03", "2025-01-15", "2025-03-02", "2025-01-20"),
  zone = c("A", "A", "B", "A"),
  stringsAsFactors = FALSE
)

test_that("aggreger_accidents compte les evenements et complete les zeros", {
  res <- aggreger_accidents(ev, "date", "zone", "mois")
  expect_equal(nrow(res), 6)
  expect_equal(sum(res$nb_accidents), 4)
  a_jan <- res$zone == "A" & res$periode == as.Date("2025-01-01")
  b_jan <- res$zone == "B" & res$periode == as.Date("2025-01-01")
  expect_equal(res$nb_accidents[a_jan], 3)
  expect_equal(res$nb_accidents[b_jan], 0)
  expect_equal(res$nb_jours[a_jan], 29)
})

test_that("completer = FALSE ne cree pas de lignes", {
  res <- aggreger_accidents(ev, "date", "zone", "mois", completer = FALSE)
  expect_equal(nrow(res), 2)
})

test_that("les noms de colonnes sont libres et les dates jj/mm/aaaa acceptees", {
  d <- data.frame(jour_obs = c("03/01/2025", "15/01/2025"),
                  ville = c("Douala", "Douala"), nombre = c(2, 3))
  res <- aggreger_accidents(d, "jour_obs", "ville", "mois", count_col = "nombre")
  expect_equal(nrow(res), 1)
  expect_equal(res$nb_accidents, 5)
  expect_equal(res$nb_observations, 2)
})

test_that("la semaine commence le lundi", {
  d <- data.frame(date = as.Date(c("2025-01-06", "2025-01-12")))
  res <- aggreger_accidents(d, "date", periode = "semaine")
  expect_equal(nrow(res), 1)
  expect_equal(res$periode, as.Date("2025-01-06"))
  expect_equal(res$nb_accidents, 2)
})

test_that("aggreger_accidents valide ses entrees", {
  expect_error(aggreger_accidents(ev, "absent", "zone"), "introuvable")
  expect_error(aggreger_accidents(ev, "date", "inconnue"), "introuvable")
  d <- data.frame(date = "2025-01-01", n = -1)
  expect_error(aggreger_accidents(d, "date", count_col = "n"), "negatives")
  expect_error(aggreger_accidents(list(1), "date"), "data.frame")
})

test_that("fonctionne sur les donnees d'exemple", {
  d <- charger_exemple_rn3()
  res <- aggreger_accidents(d, "Date", "Troncon", "mois", count_col = "Nb_Accidents")
  expect_equal(sum(res$nb_accidents), sum(d$Nb_Accidents))
})
