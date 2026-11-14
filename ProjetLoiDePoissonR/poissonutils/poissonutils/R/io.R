#' Read a data file (csv, xlsx, rds)
#'
#' Reads a data set from a file whatever its origin. For text files the
#' separator (`;`, tab or `,`) is detected automatically; with `;` the decimal
#' mark is assumed to be `,` (French-locale export) unless `dec` is given.
#'
#' @param file Path to a `.csv`, `.txt`, `.tsv`, `.xlsx`, `.xls` or `.rds` file.
#' @param sheet Sheet name or number for Excel files (default: first sheet).
#' @param dec Decimal mark for text files (`"."` or `","`); `NULL` = automatic.
#' @param encoding File encoding for text files (default `"UTF-8-BOM"`, which
#'   also reads files without a BOM).
#'
#' @return A data.frame. Column names are kept exactly as in the file.
#' @export
#' @examples
#' f <- tempfile(fileext = ".csv")
#' writeLines(c("city;number", "Douala;3", "Edea;5"), f)
#' read_data(f)
read_data <- function(file, sheet = NULL, dec = NULL, encoding = "UTF-8-BOM") {
  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    stop("`file` must be a path (character string).", call. = FALSE)
  }
  if (!file.exists(file)) {
    stop(sprintf("File not found: %s", file), call. = FALSE)
  }
  ext <- tolower(tools::file_ext(file))
  data <- switch(ext,
    csv = , txt = , tsv = {
      first <- readLines(file, n = 1L, warn = FALSE)
      seps <- c(";", "\t", ",")
      n_sep <- vapply(seps, function(s) {
        nchar(first) - nchar(gsub(s, "", first, fixed = TRUE))
      }, numeric(1))
      sep <- if (max(n_sep) == 0) "," else seps[which.max(n_sep)]
      if (is.null(dec)) dec <- if (sep == ";") "," else "."
      utils::read.table(file, header = TRUE, sep = sep, dec = dec,
                        stringsAsFactors = FALSE, check.names = FALSE,
                        quote = "\"", comment.char = "",
                        na.strings = c("NA", ""), fileEncoding = encoding)
    },
    xlsx = , xls = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Package 'readxl' is required to read Excel files: install.packages(\"readxl\").",
             call. = FALSE)
      }
      as.data.frame(readxl::read_excel(file, sheet = sheet))
    },
    rds = readRDS(file),
    stop("Unsupported format: use .csv, .txt, .tsv, .xlsx, .xls or .rds.",
         call. = FALSE)
  )
  if (!is.data.frame(data)) {
    stop("The file does not contain a data.frame.", call. = FALSE)
  }
  data
}
