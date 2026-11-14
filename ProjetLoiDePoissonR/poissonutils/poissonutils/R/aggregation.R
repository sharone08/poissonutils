#' Aggregate event counts by period (and by group)
#'
#' Turns event data (one row per event, or one row per observation with a
#' count) into counts per period and group, ready for Poisson regression.
#'
#' @param data A data.frame.
#' @param date_col Name of the date column (Date, POSIXt, or text such as
#'   `yyyy-mm-dd` or `dd/mm/yyyy`).
#' @param group_cols Names of grouping columns (e.g. zone, city), or `NULL`.
#' @param period One of `"month"` (default), `"day"`, `"week"` (starting
#'   Monday), `"quarter"` or `"year"`.
#' @param count_col Name of a count column. If `NULL`, each row is one event.
#' @param fill_zeros If `TRUE`, adds missing period x group combinations with
#'   a count of `0`. Default is `TRUE` when `count_col` is `NULL` (a missing
#'   row means zero events) and `FALSE` otherwise.
#' @param date_format Explicit date format (e.g. `"%d/%m/%Y"`); otherwise
#'   detected automatically.
#'
#' @return A data.frame with `period` (start date), the group columns,
#'   `count`, `n_obs` (only if `count_col` is given: number of rows
#'   aggregated) and `n_days` (days of the period covered by the data, usable
#'   as exposure). Rows with a missing date, count or group are dropped.
#' @export
#' @examples
#' ev <- data.frame(
#'   date = c("2025-01-03", "2025-01-15", "2025-03-02", "2025-01-20"),
#'   zone = c("A", "A", "B", "A")
#' )
#' aggregate_counts(ev, "date", "zone", "month")
aggregate_counts <- function(data, date_col, group_cols = NULL,
                             period = c("month", "day", "week",
                                        "quarter", "year"),
                             count_col = NULL,
                             fill_zeros = is.null(count_col),
                             date_format = NULL) {
  period <- match.arg(period)
  .check_df(data)
  .check_single_col(date_col, "date_col")
  .check_cols(data, date_col, "date_col")
  if (!is.null(group_cols)) .check_cols(data, group_cols, "group_cols")
  if (!is.null(count_col)) {
    .check_single_col(count_col, "count_col")
    .check_cols(data, count_col, "count_col")
  }
  reserved <- c("period", "count", "n_obs", "n_days")
  if (any(group_cols %in% reserved)) {
    stop("Group column names cannot be: ", paste(reserved, collapse = ", "), ".",
         call. = FALSE)
  }

  dates <- .as_date_robust(data[[date_col]], date_format, date_col)
  if (is.null(count_col)) {
    cnt <- rep(1, nrow(data))
  } else {
    cnt <- data[[count_col]]
    .check_counts(cnt, count_col)
  }
  keep <- !is.na(dates) & !is.na(cnt)
  if (any(!keep)) {
    warning(sprintf("%d row(s) dropped (missing date or count).", sum(!keep)),
            call. = FALSE)
  }
  if (!any(keep)) stop("No usable rows.", call. = FALSE)

  df <- data.frame(period = .period_start(dates, period), count = cnt,
                   stringsAsFactors = FALSE)
  if (!is.null(count_col)) df$n_obs <- 1
  for (g in group_cols) df[[g]] <- data[[g]]
  df <- df[keep, , drop = FALSE]

  d_min <- min(dates[keep])
  d_max <- max(dates[keep])
  value_cols <- setdiff(names(df), c("period", group_cols))
  agg <- stats::aggregate(df[value_cols], by = df[c("period", group_cols)],
                          FUN = sum)

  step <- .period_step(period)
  periods <- seq(.period_start(d_min, period), .period_start(d_max, period),
                 by = step)

  if (isTRUE(fill_zeros)) {
    grid <- data.frame(period = periods)
    if (length(group_cols) > 0L) {
      grid <- merge(grid, unique(agg[group_cols]), by = NULL)
    }
    res <- merge(grid, agg, by = c("period", group_cols), all.x = TRUE)
    for (v in value_cols) res[[v]][is.na(res[[v]])] <- 0
  } else {
    res <- agg
  }

  # Days of each period covered by the data
  nxt <- seq(periods[length(periods)], by = step, length.out = 2L)[2L]
  ends <- c(periods[-1L], nxt) - 1L
  idx <- match(res$period, periods)
  res$n_days <- as.numeric(pmin(ends[idx], d_max) - pmax(periods[idx], d_min)) + 1

  res <- res[do.call(order, unname(as.list(res[c(group_cols, "period")]))), ,
             drop = FALSE]
  rownames(res) <- NULL
  attr(res, "period") <- period
  res
}
