# Four data sets with different structures and column names

make_fr <- function() {
  set.seed(10)
  days <- seq(as.Date("2022-01-01"), by = "day", length.out = 400)
  data.frame(
    Date = format(days, "%d/%m/%Y"),
    Annee = as.integer(format(days, "%Y")),
    Mois = months(days),
    Troncon = sample(c("T1", "T2", "T3"), 400, replace = TRUE),
    Meteo = sample(c("Soleil", "Pluie"), 400, replace = TRUE),
    Trafic = sample(c("Faible", "Eleve"), 400, replace = TRUE),
    Nb_Accidents = rpois(400, 3),
    Nb_Blesses = rpois(400, 4),
    Nb_Deces = rpois(400, 0.5),
    stringsAsFactors = FALSE
  )
}
make_fr2 <- function() {
  set.seed(11)
  data.frame(jour = as.Date("2025-01-01") + 0:59,
             ville = rep(c("Douala", "Yaounde"), 30),
             nombre = rpois(60, 2),
             stringsAsFactors = FALSE)
}
make_en <- function() {
  set.seed(12)
  data.frame(crash_date = as.Date("2024-01-01") + 0:99,
             county = rep(c("North", "South"), 50),
             crashes = rpois(100, 4),
             traffic_volume = runif(100, 1000, 5000),
             stringsAsFactors = FALSE)
}
make_events <- function() {
  set.seed(13)
  data.frame(date_obs = as.character(as.Date("2025-01-01") + sample(0:59, 150, replace = TRUE)),
             region = sample(c("N", "S", "E"), 150, replace = TRUE),
             severity = sample(c("minor", "major"), 150, replace = TRUE),
             stringsAsFactors = FALSE)
}

test_that("detect_columns finds the right columns in different data sets", {
  g1 <- detect_columns(make_fr())
  expect_equal(g1$date, "Date")
  expect_equal(g1$count, "Nb_Accidents")
  expect_equal(g1$zone, "Troncon")
  expect_null(g1$exposure)

  g2 <- detect_columns(make_fr2())
  expect_equal(c(g2$date, g2$count, g2$zone), c("jour", "nombre", "ville"))

  g3 <- detect_columns(make_en())
  expect_equal(c(g3$date, g3$count, g3$zone, g3$exposure),
               c("crash_date", "crashes", "county", "traffic_volume"))

  g4 <- detect_columns(make_events())
  expect_equal(g4$date, "date_obs")
  expect_equal(g4$zone, "region")
  expect_null(g4$count)
  expect_output(print(g4), "Detected columns")
})

test_that("the same functions work on each data set with its own column names", {
  d1 <- make_fr()
  expect_equal(nrow(analyze_zones(d1, "Troncon", "Nb_Accidents")), 3)
  d2 <- make_fr2()
  expect_equal(nrow(analyze_zones(d2, "ville", "nombre")), 2)
  d3 <- make_en()
  z3 <- analyze_zones(d3, "county", "crashes", exposure_col = "traffic_volume")
  expect_equal(sum(z3$total_count), sum(d3$crashes))
  d4 <- make_events()
  expect_equal(sum(analyze_zones(d4, "region")$total_count), 150)
})

test_that("analyze_dataset adapts to each layout", {
  a1 <- analyze_dataset(make_fr(), verbose = FALSE)
  expect_s3_class(a1, "poisson_analysis")
  expect_equal(a1$columns$count, "Nb_Accidents")
  expect_equal(nrow(a1$zones), 3)
  expect_false(is.null(a1$series))
  expect_s3_class(a1$gof, "poisson_gof")
  expect_output(print(a1), "Poisson analysis")

  a3 <- analyze_dataset(make_en(), verbose = FALSE)
  expect_equal(a3$columns$exposure, "traffic_volume")

  a4 <- analyze_dataset(make_events(), verbose = FALSE)
  expect_null(a4$columns$count)
  expect_equal(nrow(a4$zones), 3)
  expect_equal(a4$lambda$total, 150)

  expect_message(analyze_dataset(make_fr2()), "Columns used")
})

test_that("analyze_dataset respects explicit columns and fails clearly", {
  d <- make_fr2()
  a <- analyze_dataset(d, count_col = "nombre", zone_col = "ville", verbose = FALSE)
  expect_equal(a$columns$zone, "ville")
  expect_error(analyze_dataset(data.frame(a = c("x", "y"), b = c("u", "v"))),
               "Could not identify")
})

test_that("read_data handles csv separators and decimal commas", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("city;number;rate", "Douala;3;1,5", "Edea;5;2,5"), f)
  d <- read_data(f)
  expect_equal(names(d), c("city", "number", "rate"))
  expect_equal(d$rate, c(1.5, 2.5))
  f2 <- tempfile(fileext = ".csv")
  writeLines(c("city,number", "Douala,3", "Edea,5"), f2)
  expect_equal(read_data(f2)$number, c(3, 5))
  f3 <- tempfile(fileext = ".rds")
  saveRDS(d, f3)
  expect_equal(read_data(f3), d)
  expect_error(read_data(tempfile(fileext = ".csv")), "not found")
})

test_that("analyze_dataset reads a file path directly", {
  f <- tempfile(fileext = ".csv")
  write.csv(make_fr2(), f, row.names = FALSE)
  a <- analyze_dataset(f, verbose = FALSE)
  expect_equal(a$columns$count, "nombre")
})
