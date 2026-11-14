# Demonstration on simulated data
library(poissonutils)

# 1. The simple one: probability of exactly k events
poisson_prob(2, lambda = 3)                       # P(X = 2)
poisson_prob(2, lambda = 3, type = "cumulative")  # P(X <= 2)
poisson_table(lambda = 3, k_max = 10, days = 365)

# 2. Simulate daily event data for 3 road sections
set.seed(1)
days <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
d <- data.frame(day = rep(days, 3),
                section = rep(c("S1", "S2", "S3"), each = length(days)))
d$rain <- rbinom(nrow(d), 1, 0.3)
d$n <- rpois(nrow(d), exp(0.5 + 0.4 * d$rain + 0.3 * (d$section == "S3")))

# 3. Estimation and goodness of fit
estimate_lambda(d$n)
poisson_gof_test(d$n)

# 4. Zones
zones <- analyze_zones(d, zone_col = "section", count_col = "n")
zones

# 5. Aggregation, model, diagnostic, prediction
monthly <- aggregate_counts(d, "day", "section", "month", count_col = "n")
model <- fit_poisson_model(d, n ~ rain + section)
check_overdispersion(model)
predict_counts(model, data.frame(rain = 1, section = "S3"))

# 6. Plot, alerts, export
plot_counts(zones, "zone", "total_count")
generate_alerts(zones, "total_count", threshold = alert_threshold(mean(d$n) * 365))
export_results(list(zones = zones, monthly = monthly), "results.xlsx")
