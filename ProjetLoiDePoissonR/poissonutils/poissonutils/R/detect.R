# Normalise column names: lower case, ASCII, words separated by "_"
#' @noRd
.norm_names <- function(x) {
  y <- iconv(x, to = "ASCII//TRANSLIT")
  y[is.na(y)] <- x[is.na(y)]
  tolower(gsub("[^A-Za-z0-9]+", "_", y))
}

#' Guess the role of each column
#'
#' Looks at column types, values and names (English and French words) to
#' suggest which column holds the date, the event count, the zone and the
#' exposure. It is only a suggestion: always check it, and give the column
#' names explicitly to the other functions when it is wrong or missing.
#'
#' @param data A data.frame.
#'
#' @return An object of class `column_guess` (a list) with elements `date`,
#'   `count`, `zone` and `exposure`: a column name, or `NULL` if nothing
#'   suitable was found. A `count` is only proposed for data where each row
#'   already holds a count; for one-row-per-event data it is `NULL`.
#' @export
#' @examples
#' d <- data.frame(day = as.Date("2025-01-01") + 0:5,
#'                 city = rep(c("A", "B"), 3),
#'                 number = c(2, 0, 3, 1, 4, 2))
#' detect_columns(d)
detect_columns <- function(data) {
  .check_df(data)
  nms <- names(data)
  norm <- .norm_names(nms)
  p <- seq_along(data)
  has_sub <- function(i, pat) grepl(pat, norm[i])
  has_tok <- function(i, set) any(strsplit(norm[i], "_+")[[1L]] %in% set)

  # ---- date ----
  date_score <- vapply(p, function(i) {
    x <- data[[i]]
    s <- 0
    if (inherits(x, c("Date", "POSIXt"))) {
      s <- 3
    } else if (is.character(x) || is.factor(x)) {
      xs <- as.character(x)
      ok <- !is.na(xs) & nzchar(trimws(xs))
      if (any(ok) && mean(ok) > 0.5) {
        parsed <- tryCatch(.as_date_robust(xs), error = function(e) NULL)
        if (!is.null(parsed)) s <- 2
      }
    }
    if (s > 0 && (has_sub(i, "date|jour|day|time|fecha") || has_tok(i, c("dt", "d")))) {
      s <- s + 1
    }
    s
  }, numeric(1))
  date_col <- if (max(date_score) > 0) nms[which.max(date_score)] else NULL

  # ---- count (rows that already hold a count) ----
  is_count <- vapply(p, function(i) {
    x <- data[[i]]
    if (!is.numeric(x) || inherits(x, c("Date", "POSIXt"))) return(FALSE)
    v <- x[!is.na(x)]
    length(v) > 0L && all(is.finite(v)) && all(v >= 0) && all(abs(v - round(v)) < 1e-8)
  }, logical(1))
  excluded <- vapply(p, function(i) {
    has_tok(i, c("id", "code", "year", "annee", "month", "mois", "week", "semaine",
                 "day", "jour", "age", "rank", "rang", "lat", "lon",
                 "latitude", "longitude")) ||
      has_sub(i, "bless|injur|death|deces|mort|fatal|killed|victim")
  }, logical(1))
  count_score <- vapply(p, function(i) {
    has_sub(i, "accident|crash|incident|collision|event|count|nombre|number|cases") +
      has_tok(i, c("nb", "n", "freq", "frequency", "total"))
  }, numeric(1))
  count_score[!is_count | excluded] <- 0
  count_col <- if (max(count_score) > 0) nms[which.max(count_score)] else NULL

  # ---- zone ----
  is_zone <- vapply(p, function(i) {
    x <- data[[i]]
    (is.character(x) || is.factor(x)) && !identical(nms[i], date_col) &&
      length(unique(x[!is.na(x)])) >= 2L &&
      has_sub(i, paste0("zone|troncon|section|segment|road|route|axe|ville|city|town|",
                        "region|county|location|lieu|site|district|commune|area|",
                        "secteur|quartier|province|department|departement"))
  }, logical(1))
  zone_col <- if (any(is_zone)) nms[which(is_zone)[1L]] else NULL

  # ---- exposure ----
  is_expo <- vapply(p, function(i) {
    x <- data[[i]]
    is.numeric(x) && !inherits(x, c("Date", "POSIXt")) && !anyNA(x) &&
      length(x) > 0L && all(x > 0) && !identical(nms[i], count_col) &&
      (has_sub(i, "exposure|exposition|traffic|trafic|volume|length|longueur|vehic|population|aadt|tmja|distance") ||
         has_tok(i, c("km", "days", "jours", "pop", "vol")))
  }, logical(1))
  expo_col <- if (any(is_expo)) nms[which(is_expo)[1L]] else NULL

  structure(
    list(date = date_col, count = count_col, zone = zone_col, exposure = expo_col),
    class = "column_guess"
  )
}

#' @export
print.column_guess <- function(x, ...) {
  show <- function(v) if (is.null(v)) "<not found>" else v
  cat("Detected columns\n")
  cat("  date    :", show(x$date), "\n")
  cat("  count   :", show(x$count), "\n")
  cat("  zone    :", show(x$zone), "\n")
  cat("  exposure:", show(x$exposure), "\n")
  invisible(x)
}
