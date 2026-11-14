test_that("analyze_zones ranks zones correctly", {
  d <- data.frame(zone = rep(c("A", "B", "C"), each = 100),
                  n = c(rep(1, 100), rep(1, 100), rep(5, 100)))
  res <- analyze_zones(d, "zone", count_col = "n")
  expect_equal(res$zone[1], "C")
  expect_equal(res$rank[1], 1)
  expect_equal(res$classification[res$zone == "C"], "High risk")
  expect_equal(res$classification[res$zone == "A"], "Low risk")
  expect_equal(sum(res$share), 1)
  expect_equal(res$risk_score[1], 10)
})

test_that("equivalent zones are classified as average", {
  d <- data.frame(zone = rep(c("A", "B"), each = 50), n = 3)
  res <- analyze_zones(d, "zone", count_col = "n")
  expect_true(all(res$classification == "Average"))
})

test_that("event-level data and free column names work", {
  d <- data.frame(city = c(rep("X", 30), rep("Y", 10)))
  res <- analyze_zones(d, "city")
  expect_equal(res$total_count[res$zone == "X"], 30)
  expect_equal(res$share[res$zone == "X"], 0.75)
  d2 <- data.frame(obs = 1:4, city = c("a", "a", "b", "b"), number = c(1, 2, 3, 4))
  res2 <- analyze_zones(d2, "city", "number")
  expect_equal(res2$total_count[res2$zone == "b"], 7)
})

test_that("exposure is taken into account", {
  d <- data.frame(z = c("A", "B"), n = c(10, 10), km = c(10, 100))
  res <- analyze_zones(d, "z", "n", exposure_col = "km")
  expect_equal(res$rate[res$zone == "A"], 1)
  expect_equal(res$rank[res$zone == "A"], 1)
})

test_that("analyze_zones validates its inputs", {
  d <- data.frame(z = c("A", "B"), n = c(1, 2))
  expect_error(analyze_zones(d, "missing"), "not found")
  expect_error(analyze_zones(d, "z", "n", alpha = 2))
  expect_error(analyze_zones(data.frame(z = "A", n = 0), "z", "n"), "No events")
})
