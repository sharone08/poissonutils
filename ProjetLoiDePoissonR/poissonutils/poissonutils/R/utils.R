# Internal validation helpers (not exported)

#' @noRd
.check_df <- function(data, arg = "data") {
  if (!is.data.frame(data)) {
    stop(sprintf("`%s` must be a data.frame.", arg), call. = FALSE)
  }
  if (nrow(data) == 0L) {
    stop(sprintf("`%s` has no rows.", arg), call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
.check_cols <- function(data, cols, arg = "col") {
  if (!is.character(cols) || length(cols) == 0L || anyNA(cols)) {
    stop(sprintf("`%s` must be a column name (character string).", arg),
         call. = FALSE)
  }
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0L) {
    stop(sprintf("Column(s) not found for `%s`: %s. Available columns: %s.",
                 arg, paste(missing_cols, collapse = ", "),
                 paste(names(data), collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
.check_single_col <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be a single column name (character string).", arg),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
.check_level <- function(x, arg = "level") {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x <= 0 || x >= 1) {
    stop(sprintf("`%s` must be a number strictly between 0 and 1.", arg),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
.check_lambda <- function(lambda) {
  if (!is.numeric(lambda) || length(lambda) == 0L || anyNA(lambda) ||
      any(!is.finite(lambda)) || any(lambda < 0)) {
    stop("`lambda` must be numeric, finite and non-negative.", call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
.check_counts <- function(x, arg = "x") {
  if (!is.numeric(x)) {
    stop(sprintf("`%s` must be numeric (counts).", arg), call. = FALSE)
  }
  v <- x[!is.na(x)]
  if (any(!is.finite(v))) {
    stop(sprintf("`%s` contains infinite values.", arg), call. = FALSE)
  }
  if (any(v < 0)) {
    stop(sprintf("`%s` contains negative values.", arg), call. = FALSE)
  }
  if (any(abs(v - round(v)) > 1e-8)) {
    stop(sprintf("`%s` must contain whole numbers (counts).", arg), call. = FALSE)
  }
  invisible(x)
}

# Robust conversion to Date: Date, POSIXt or text (ISO, dd/mm/yyyy, ...)
#' @noRd
.as_date_robust <- function(x, format = NULL, arg = "date_col") {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    non_na <- !is.na(x) & nzchar(trimws(x))
    formats <- if (is.null(format)) {
      c("%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d", "%d-%m-%Y")
    } else {
      format
    }
    for (f in formats) {
      d <- as.Date(x, format = f)
      if (all(!is.na(d[non_na]))) {
        d[!non_na] <- NA
        return(d)
      }
    }
    stop(sprintf(
      "Cannot convert `%s` to dates. Set `date_format` (e.g. \"%%d/%%m/%%Y\").",
      arg), call. = FALSE)
  }
  stop(sprintf("`%s` must be a Date, POSIXt or character column.", arg),
       call. = FALSE)
}

# Start date of the period containing each date
#' @noRd
.period_start <- function(d, period) {
  switch(period,
    day = d,
    week = d - (as.integer(format(d, "%u")) - 1L),
    month = as.Date(format(d, "%Y-%m-01")),
    quarter = {
      m <- as.integer(format(d, "%m"))
      as.Date(sprintf("%s-%02d-01", format(d, "%Y"), 3L * ((m - 1L) %/% 3L) + 1L))
    },
    year = as.Date(paste0(format(d, "%Y"), "-01-01"))
  )
}

#' @noRd
.period_step <- function(period) {
  c(day = "day", week = "week", month = "month",
    quarter = "quarter", year = "year")[[period]]
}
