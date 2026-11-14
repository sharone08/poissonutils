#' Run a complete Poisson analysis on any data set
#'
#' One call that adapts to the data it receives. Columns that are not given
#' are guessed with [detect_columns()]; then the function estimates lambda,
#' tests the Poisson fit, ranks the zones and aggregates counts over time,
#' skipping any step the data cannot support.
#'
#' Two data layouts are handled: rows that already hold a count (`count_col`
#' found or given) and one row per event (`count_col` is `NULL`; counts per
#' day are then built from `date_col`).
#'
#' @param data A data.frame, or a path to a csv / xlsx / rds file
#'   (read with [read_data()]).
#' @param date_col,count_col,zone_col,exposure_col Column names. `NULL`
#'   means "guess it". To force one-row-per-event data, use
#'   `count_col = NA`.
#' @param period Aggregation period for the time series (see
#'   [aggregate_counts()]).
#' @param sheet Excel sheet, when `data` is an Excel file.
#' @param verbose If `TRUE`, reports the columns used.
#'
#' @return An object of class `poisson_analysis` (a list) with `columns`,
#'   `lambda`, `gof` (or `NULL`), `zones` (or `NULL`), `series` (or `NULL`)
#'   and `probabilities`.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(day = rep(as.Date("2025-01-01") + 0:99, 2),
#'                 section = rep(c("S1", "S2"), each = 100))
#' d$n <- rpois(200, 3)
#' analyze_dataset(d)
analyze_dataset <- function(data, date_col = NULL, count_col = NULL,
                            zone_col = NULL, exposure_col = NULL,
                            period = c("month", "day", "week", "quarter", "year"),
                            sheet = NULL, verbose = TRUE) {
  period <- match.arg(period)
  if (is.character(data) && length(data) == 1L) data <- read_data(data, sheet = sheet)
  .check_df(data)

  guess <- detect_columns(data)
  if (is.null(date_col)) date_col <- guess$date
  if (is.null(count_col)) count_col <- guess$count
  if (isTRUE(is.na(count_col))) count_col <- NULL
  if (is.null(zone_col)) zone_col <- guess$zone
  if (is.null(exposure_col)) exposure_col <- guess$exposure

  if (is.null(count_col) && is.null(date_col)) {
    stop("Could not identify a count column or a date column: ",
         "set `count_col` or `date_col`.", call. = FALSE)
  }
  if (isTRUE(verbose)) {
    show <- function(v) if (is.null(v)) "none" else v
    message(sprintf("Columns used -> date: %s | count: %s | zone: %s | exposure: %s",
                    show(date_col), show(count_col), show(zone_col), show(exposure_col)))
    if (is.null(count_col)) {
      message("No count column: each row is treated as one event.")
    }
  }

  if (!is.null(count_col)) {
    x <- data[[count_col]]
  } else {
    x <- aggregate_counts(data, date_col, period = "day")$count
  }
  lambda <- estimate_lambda(x)
  gof <- tryCatch(poisson_gof_test(x), error = function(e) NULL,
                  warning = function(w) NULL)
  zones <- if (!is.null(zone_col)) {
    analyze_zones(data, zone_col, count_col, exposure_col)
  } else {
    NULL
  }
  series <- if (!is.null(date_col)) {
    aggregate_counts(data, date_col, period = period, count_col = count_col)
  } else {
    NULL
  }
  k_max <- max(10, ceiling(max(x, na.rm = TRUE)))
  structure(
    list(
      columns = list(date = date_col, count = count_col, zone = zone_col,
                     exposure = exposure_col),
      lambda = lambda, gof = gof, zones = zones, series = series,
      probabilities = poisson_table(lambda$lambda, k_max)
    ),
    class = "poisson_analysis"
  )
}

#' @export
print.poisson_analysis <- function(x, ...) {
  cols <- unlist(x$columns)
  cat("Poisson analysis\n")
  cat("Columns:", if (length(cols)) paste(names(cols), cols, sep = " = ", collapse = ", ")
      else "none", "\n")
  cat(sprintf("lambda = %.3f  (95%% CI %.3f - %.3f) | dispersion index = %.2f\n",
              x$lambda$lambda, x$lambda$ci_lower, x$lambda$ci_upper,
              x$lambda$dispersion_index))
  if (!is.null(x$gof)) cat("Poisson fit:", x$gof$conclusion, "\n")
  if (!is.null(x$zones)) {
    cat("\nTop zones:\n")
    print(utils::head(x$zones[c("zone", "total_count", "rate", "classification")], 5L),
          row.names = FALSE)
  }
  invisible(x)
}
