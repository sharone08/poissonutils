test_that("generate_alerts assigns levels", {
  z <- data.frame(zone = c("A", "B", "C"), total = c(5, 12, 20))
  r <- generate_alerts(z, "total", threshold = 10)
  expect_equal(r$alert_level,
               c("Routine monitoring", "Increased vigilance", "Increased vigilance"))
  r2 <- generate_alerts(z, "total", threshold = 10, critical_threshold = 15)
  expect_equal(r2$alert_level[3], "Critical alert")
  expect_true(all(c("alert_level", "recommendation") %in% names(r2)))
  expect_error(generate_alerts(z, "total", threshold = 10, critical_threshold = 5))
  expect_error(generate_alerts(z, "missing", threshold = 10), "not found")
})

test_that("generate_alerts handles missing values", {
  z <- data.frame(total = c(NA, 12))
  r <- generate_alerts(z, "total", threshold = 10)
  expect_equal(r$alert_level[1], "Missing data")
})

test_that("alert_threshold is consistent with Poisson", {
  s <- alert_threshold(3, 0.95)
  expect_equal(s, 7)
  expect_lte(ppois(s - 1, 3, lower.tail = FALSE), 0.05)
})

test_that("plot_counts returns a ggplot", {
  skip_if_not_installed("ggplot2")
  z <- data.frame(zone = c("A", "B", "C"), total_count = c(12, 30, 7))
  expect_s3_class(plot_counts(z, "zone", "total_count"), "ggplot")
  expect_s3_class(
    plot_counts(z, "zone", "total_count", type = "point", horizontal = TRUE),
    "ggplot")
  expect_error(plot_counts(z, "zone", "missing"), "not found")
  expect_error(plot_counts(z, "total_count", "zone"), "numeric")
})

test_that("export_results writes csv, rds and xlsx", {
  d <- data.frame(a = 1:3, b = c("x", "y", "z"))
  f_csv <- tempfile(fileext = ".csv")
  export_results(d, f_csv)
  expect_equal(nrow(read.csv(f_csv)), 3)
  expect_error(export_results(d, f_csv, overwrite = FALSE), "exists")
  f_rds <- tempfile(fileext = ".rds")
  export_results(d, f_rds)
  expect_equal(readRDS(f_rds), d)
  expect_error(export_results(d, tempfile(fileext = ".txt")), "Unsupported")
  skip_if_not_installed("writexl")
  f_x <- tempfile(fileext = ".xlsx")
  export_results(list(one = d, two = d), f_x)
  expect_true(file.exists(f_x))
})
