#' Plot counts
#'
#' @param data A data.frame (e.g. the result of [analyze_zones()] or
#'   [aggregate_counts()]).
#' @param x_col Column for the x axis.
#' @param y_col Numeric column for the y axis.
#' @param type `"bar"`, `"line"` or `"point"`.
#' @param group_col Grouping column (colour), or `NULL`.
#' @param title,xlab,ylab Plot title and axis labels.
#' @param sort If `TRUE`, sorts bars by decreasing value (ungrouped bars with
#'   a categorical x axis).
#' @param horizontal If `TRUE`, draws horizontal bars.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' z <- data.frame(zone = c("A", "B", "C"), total_count = c(12, 30, 7))
#' plot_counts(z, "zone", "total_count", title = "Events by zone")
plot_counts <- function(data, x_col, y_col,
                        type = c("bar", "line", "point"),
                        group_col = NULL, title = NULL,
                        xlab = x_col, ylab = y_col,
                        sort = TRUE, horizontal = FALSE) {
  type <- match.arg(type)
  .check_df(data)
  .check_single_col(x_col, "x_col")
  .check_single_col(y_col, "y_col")
  .check_cols(data, c(x_col, y_col, group_col), "x_col/y_col/group_col")
  if (!is.numeric(data[[y_col]])) {
    stop("`y_col` must be a numeric column.", call. = FALSE)
  }
  d <- data
  x <- d[[x_col]]
  if (type == "bar" && isTRUE(sort) && is.null(group_col) &&
      !is.numeric(x) && !inherits(x, c("Date", "POSIXt"))) {
    d[[x_col]] <- stats::reorder(factor(x), -d[[y_col]], FUN = sum)
  }
  colour <- "#1F78B4"
  if (is.null(group_col)) {
    mapping <- ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])
  } else {
    mapping <- ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]],
                            colour = .data[[group_col]],
                            fill = .data[[group_col]],
                            group = .data[[group_col]])
  }
  p <- ggplot2::ggplot(d, mapping)
  layer <- switch(type,
    bar = if (is.null(group_col)) ggplot2::geom_col(fill = colour) else
      ggplot2::geom_col(position = "dodge"),
    line = if (is.null(group_col)) {
      list(ggplot2::geom_line(colour = colour), ggplot2::geom_point(colour = colour))
    } else {
      list(ggplot2::geom_line(), ggplot2::geom_point())
    },
    point = if (is.null(group_col)) ggplot2::geom_point(colour = colour, size = 2.5) else
      ggplot2::geom_point(size = 2.5)
  )
  p <- p + layer +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme_minimal()
  if (isTRUE(horizontal)) p <- p + ggplot2::coord_flip()
  p
}

#' Alert threshold from the Poisson distribution
#'
#' Smallest count considered unusually high: a count `x` is unusual if
#' \eqn{P(X \ge x) \le 1 - level} under Poisson(lambda).
#'
#' @param lambda Expected average count (numeric `>= 0`).
#' @param level Probability level (default `0.95`).
#'
#' @return A numeric vector of thresholds.
#' @export
#' @examples
#' alert_threshold(3, 0.95)
alert_threshold <- function(lambda, level = 0.95) {
  .check_lambda(lambda)
  .check_level(level)
  stats::qpois(level, lambda) + 1
}

#' Generate preventive alerts
#'
#' @param data A data.frame.
#' @param count_col Name of the count column.
#' @param threshold Threshold for increased vigilance (`x >= threshold`).
#' @param critical_threshold Optional threshold for a critical alert
#'   (`>= threshold`).
#'
#' @return `data` with two extra columns: `alert_level` and `recommendation`.
#' @export
#' @examples
#' z <- data.frame(zone = c("A", "B", "C"), total_count = c(5, 12, 20))
#' generate_alerts(z, "total_count", threshold = 10, critical_threshold = 15)
generate_alerts <- function(data, count_col, threshold, critical_threshold = NULL) {
  .check_df(data)
  .check_single_col(count_col, "count_col")
  .check_cols(data, count_col, "count_col")
  x <- data[[count_col]]
  if (!is.numeric(x)) stop("`count_col` must be numeric.", call. = FALSE)
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold)) {
    stop("`threshold` must be a number.", call. = FALSE)
  }
  if (!is.null(critical_threshold)) {
    if (!is.numeric(critical_threshold) || length(critical_threshold) != 1L ||
        is.na(critical_threshold) || critical_threshold < threshold) {
      stop("`critical_threshold` must be a number >= `threshold`.", call. = FALSE)
    }
  }
  level <- ifelse(x >= threshold, "Increased vigilance", "Routine monitoring")
  reco <- ifelse(x >= threshold,
                 "Examine the circumstances and strengthen monitoring",
                 "Maintain monitoring")
  if (!is.null(critical_threshold)) {
    crit <- !is.na(x) & x >= critical_threshold
    level[crit] <- "Critical alert"
    reco[crit] <- "Priority action: analyze the causes and act without delay"
  }
  miss <- is.na(x)
  level[miss] <- "Missing data"
  reco[miss] <- "Check the data"
  data$alert_level <- level
  data$recommendation <- reco
  data
}

#' Export results
#'
#' @param data A data.frame, or a named list of data.frames (one sheet per
#'   element for `xlsx`).
#' @param file File path; the format is taken from the extension: `.csv`,
#'   `.xlsx` (needs the `writexl` package) or `.rds`.
#' @param overwrite If `FALSE`, refuses to overwrite an existing file.
#' @param semicolon For CSV: use `;` as separator and `,` as decimal mark
#'   (French-locale Excel).
#'
#' @return The path of the written file (invisibly).
#' @export
#' @examples
#' f <- tempfile(fileext = ".csv")
#' export_results(data.frame(a = 1:3), f)
export_results <- function(data, file, overwrite = TRUE, semicolon = FALSE) {
  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    stop("`file` must be a path (character string).", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(file))
  if (!ext %in% c("csv", "xlsx", "rds")) {
    stop("Unsupported format: use .csv, .xlsx or .rds.", call. = FALSE)
  }
  if (is.data.frame(data)) {
    items <- list(Results = data)
  } else if (is.list(data) && length(data) > 0L &&
             all(vapply(data, is.data.frame, logical(1)))) {
    items <- data
    if (is.null(names(items)) || any(!nzchar(names(items)))) {
      names(items) <- paste0("Sheet", seq_along(items))
    }
  } else {
    stop("`data` must be a data.frame or a list of data.frames.", call. = FALSE)
  }
  if (file.exists(file) && !isTRUE(overwrite)) {
    stop("File already exists (use overwrite = TRUE).", call. = FALSE)
  }
  dir <- dirname(file)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  if (ext == "csv") {
    if (length(items) > 1L) {
      stop("CSV holds a single table; use .xlsx or .rds for several.", call. = FALSE)
    }
    writer <- if (isTRUE(semicolon)) utils::write.csv2 else utils::write.csv
    writer(items[[1L]], file = file, row.names = FALSE, fileEncoding = "UTF-8")
  } else if (ext == "xlsx") {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop("Package 'writexl' is required for xlsx export: install.packages(\"writexl\").",
           call. = FALSE)
    }
    writexl::write_xlsx(items, path = file)
  } else {
    saveRDS(if (is.data.frame(data)) data else items, file)
  }
  invisible(normalizePath(file))
}
