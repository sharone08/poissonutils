# poissonutils

Tools for count data under the Poisson model. Every function works on **any data.frame**:
you give the column names.

```r
# Probability of exactly 2 events when the average is 3
poisson_prob(2, lambda = 3)

analyze_zones(my_data, zone_col = "city", count_col = "number")
```

## Works on any data set

```r
df <- read_data("my_file.xlsx")      # csv / xlsx / rds, separators detected
detect_columns(df)                   # suggests date / count / zone / exposure columns
analyze_dataset(df)                  # full analysis, columns guessed if not given

# or be explicit (always possible, always wins over the guess)
analyze_dataset(df, date_col = "day", count_col = "number", zone_col = "city")
```

Two layouts are supported: rows that already hold a count (`count_col`), or one row per
event (`count_col = NULL`).

## Functions

| Function | Purpose |
|---|---|
| `read_data()` | Read csv / xlsx / rds |
| `detect_columns()` | Guess date, count, zone, exposure columns |
| `analyze_dataset()` | Full analysis in one call |
| `poisson_prob()` | P(X = k), P(X <= k) or P(X > k) |
| `poisson_table()` | Table of probabilities (+ expected days) |
| `estimate_lambda()` | Rate, exact CI, dispersion index |
| `poisson_gof_test()` | Chi-squared goodness-of-fit test |
| `aggregate_counts()` | Counts by period and group |
| `analyze_zones()` | Ranking and test of risky zones |
| `fit_poisson_model()` | Poisson regression (exposure offset) |
| `predict_counts()` | Predictions with confidence interval |
| `check_overdispersion()` | Pearson + Cameron-Trivedi |
| `plot_counts()` | ggplot2 charts |
| `alert_threshold()` / `generate_alerts()` | Preventive alerts |
| `export_results()` | Export to csv / xlsx / rds |

## Develop and check

```r
install.packages(c("devtools", "roxygen2", "testthat", "ggplot2", "rlang", "writexl"))
devtools::document()   # generates man/ and NAMESPACE
devtools::load_all()
devtools::test()
devtools::check()
devtools::install()
```

Demo script: `inst/scripts/demo.R`.

## Notes

* Regression needs **aggregated counts** (use `aggregate_counts()` for event-level data).
* The zone test assumes independent observations; under overdispersion its p-values are
  too optimistic (see `check_overdispersion()`).
