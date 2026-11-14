ev <- data.frame(
  date = c("2025-01-03", "2025-01-15", "2025-03-02", "2025-01-20"),
  zone = c("A", "A", "B", "A"),
  stringsAsFactors = FALSE
)

test_that("aggregate_counts counts events and fills zeros", {
  res <- aggregate_counts(ev, "date", "zone", "month")
  expect_equal(nrow(res), 6)
  expect_equal(sum(res$count), 4)
  a_jan <- res$zone == "A" & res$period == as.Date("2025-01-01")
  b_jan <- res$zone == "B" & res$period == as.Date("2025-01-01")
  expect_equal(res$count[a_jan], 3)
  expect_equal(res$count[b_jan], 0)
  expect_equal(res$n_days[a_jan], 29)
})

test_that("fill_zeros = FALSE does not create rows", {
  res <- aggregate_counts(ev, "date", "zone", "month", fill_zeros = FALSE)
  expect_equal(nrow(res), 2)
})

test_that("column names are free and dd/mm/yyyy dates are accepted", {
  d <- data.frame(obs_day = c("03/01/2025", "15/01/2025"),
                  city = c("Douala", "Douala"), number = c(2, 3))
  res <- aggregate_counts(d, "obs_day", "city", "month", count_col = "number")
  expect_equal(nrow(res), 1)
  expect_equal(res$count, 5)
  expect_equal(res$n_obs, 2)
})

test_that("weeks start on Monday", {
  d <- data.frame(date = as.Date(c("2025-01-06", "2025-01-12")))
  res <- aggregate_counts(d, "date", period = "week")
  expect_equal(nrow(res), 1)
  expect_equal(res$period, as.Date("2025-01-06"))
  expect_equal(res$count, 2)
})

test_that("aggregate_counts validates its inputs", {
  expect_error(aggregate_counts(ev, "missing", "zone"), "not found")
  expect_error(aggregate_counts(ev, "date", "unknown"), "not found")
  d <- data.frame(date = "2025-01-01", n = -1)
  expect_error(aggregate_counts(d, "date", count_col = "n"), "negative")
  expect_error(aggregate_counts(list(1), "date"), "data.frame")
})

test_that("works on a simulated daily data set", {
  set.seed(3)
  d <- data.frame(day = seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"))
  d$section <- sample(c("S1", "S2", "S3"), nrow(d), replace = TRUE)
  d$n <- rpois(nrow(d), 3)
  res <- aggregate_counts(d, "day", "section", "month", count_col = "n")
  expect_equal(sum(res$count), sum(d$n))
})
