#' Analyze risky zones
#'
#' Ranks zones (road sections, cities, ...) by event rate and tests, for each
#' one, whether its rate differs from the overall rate (exact two-sided
#' Poisson test, with multiple-comparison adjustment).
#'
#' Exposure of each zone is: the sum of `exposure_col` if given; otherwise the
#' number of rows of the zone if `count_col` is given (one row = one
#' observation unit, e.g. one day); otherwise (one row = one event) zones are
#' assumed to have equal exposure.
#'
#' @param data A data.frame.
#' @param zone_col Name of the column identifying the zone.
#' @param count_col Name of a count column. If `NULL`, each row is one event.
#' @param exposure_col Name of an exposure column (traffic, length, days...),
#'   numeric and strictly positive, or `NULL`.
#' @param alpha Significance level after adjustment (default `0.05`).
#' @param adjust Method passed to [stats::p.adjust()] (default `"holm"`).
#'
#' @return A data.frame sorted by rank: `zone`, `n_obs`, `total_count`,
#'   `share`, `exposure`, `rate`, `rate_ratio` (rate / overall rate),
#'   `p_value`, `p_adjusted`, `rank`, `risk_score` (0-10, relative to the
#'   highest rate) and `classification` (`"High risk"`, `"Low risk"` or
#'   `"Average"`). The overall rate is stored in attribute `overall_rate`.
#' @export
#' @examples
#' d <- data.frame(city = rep(c("A", "B", "C"), each = 50),
#'                 n = c(rep(1, 50), rep(1, 50), rep(4, 50)))
#' analyze_zones(d, zone_col = "city", count_col = "n")
analyze_zones <- function(data, zone_col, count_col = NULL,
                          exposure_col = NULL, alpha = 0.05,
                          adjust = "holm") {
  .check_df(data)
  .check_single_col(zone_col, "zone_col")
  .check_cols(data, zone_col, "zone_col")
  .check_level(alpha, "alpha")
  if (!is.character(adjust) || length(adjust) != 1L ||
      !adjust %in% stats::p.adjust.methods) {
    stop("`adjust` must be a method of stats::p.adjust().", call. = FALSE)
  }

  zone <- data[[zone_col]]
  if (is.null(count_col)) {
    cnt <- rep(1, nrow(data))
  } else {
    .check_single_col(count_col, "count_col")
    .check_cols(data, count_col, "count_col")
    cnt <- data[[count_col]]
    .check_counts(cnt, count_col)
  }
  if (is.null(exposure_col)) {
    e <- rep(1, nrow(data))
  } else {
    .check_single_col(exposure_col, "exposure_col")
    .check_cols(data, exposure_col, "exposure_col")
    e <- data[[exposure_col]]
    if (!is.numeric(e) || any(e[!is.na(e)] <= 0)) {
      stop("`exposure_col` must be numeric and strictly positive.", call. = FALSE)
    }
  }

  keep <- !is.na(zone) & !is.na(cnt) & !is.na(e)
  if (any(!keep)) {
    warning(sprintf("%d row(s) dropped (missing zone, count or exposure).",
                    sum(!keep)), call. = FALSE)
  }
  if (!any(keep)) stop("No usable rows.", call. = FALSE)
  zone <- droplevels(as.factor(zone[keep]))
  cnt <- cnt[keep]
  e <- e[keep]

  total <- as.numeric(tapply(cnt, zone, sum))
  n_obs <- as.integer(table(zone))
  if (sum(total) == 0) stop("No events in the data.", call. = FALSE)
  expo <- if (!is.null(exposure_col)) {
    as.numeric(tapply(e, zone, sum))
  } else if (!is.null(count_col)) {
    as.numeric(n_obs)
  } else {
    rep(1, length(total))
  }

  overall_rate <- sum(total) / sum(expo)
  rate <- total / expo
  expected <- overall_rate * expo
  p_high <- stats::ppois(total - 1, expected, lower.tail = FALSE)
  p_low <- stats::ppois(total, expected)
  p <- pmin(1, 2 * pmin(p_high, p_low))
  p_adj <- stats::p.adjust(p, method = adjust)

  classification <- ifelse(
    p_adj < alpha & rate > overall_rate, "High risk",
    ifelse(p_adj < alpha & rate < overall_rate, "Low risk", "Average")
  )
  res <- data.frame(
    zone = levels(zone),
    n_obs = n_obs,
    total_count = total,
    share = total / sum(total),
    exposure = expo,
    rate = rate,
    rate_ratio = rate / overall_rate,
    p_value = p,
    p_adjusted = p_adj,
    rank = rank(-rate, ties.method = "min"),
    risk_score = rate / max(rate) * 10,
    classification = classification,
    stringsAsFactors = FALSE
  )
  res <- res[order(res$rank, res$zone), , drop = FALSE]
  rownames(res) <- NULL
  attr(res, "overall_rate") <- overall_rate
  res
}
