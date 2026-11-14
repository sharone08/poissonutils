# Fonctions internes de validation (non exportees)

#' @noRd
.verifier_dataframe <- function(data, arg = "data") {
  if (!is.data.frame(data)) {
    stop(sprintf("`%s` doit etre un data.frame.", arg), call. = FALSE)
  }
  if (nrow(data) == 0L) {
    stop(sprintf("`%s` ne contient aucune ligne.", arg), call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
.verifier_colonnes <- function(data, cols, arg = "col") {
  if (!is.character(cols) || length(cols) == 0L || anyNA(cols)) {
    stop(sprintf("`%s` doit etre un nom de colonne (chaine de caracteres).", arg),
         call. = FALSE)
  }
  manquantes <- setdiff(cols, names(data))
  if (length(manquantes) > 0L) {
    stop(sprintf(
      "Colonne(s) introuvable(s) pour `%s` : %s. Colonnes disponibles : %s.",
      arg, paste(manquantes, collapse = ", "), paste(names(data), collapse = ", ")),
      call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
.verifier_scalaire_chr <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` doit etre un seul nom de colonne (chaine de caracteres).", arg),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
.verifier_niveau <- function(niveau, arg = "niveau") {
  if (!is.numeric(niveau) || length(niveau) != 1L || is.na(niveau) ||
      niveau <= 0 || niveau >= 1) {
    stop(sprintf("`%s` doit etre un nombre strictement entre 0 et 1.", arg),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
.verifier_lambda <- function(lambda) {
  if (!is.numeric(lambda) || length(lambda) == 0L || anyNA(lambda) ||
      any(!is.finite(lambda)) || any(lambda < 0)) {
    stop("`lambda` doit etre numerique, fini et positif ou nul.", call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
.verifier_comptage <- function(x, arg = "x") {
  if (!is.numeric(x)) {
    stop(sprintf("`%s` doit etre numerique (comptage).", arg), call. = FALSE)
  }
  v <- x[!is.na(x)]
  if (any(!is.finite(v))) {
    stop(sprintf("`%s` contient des valeurs infinies.", arg), call. = FALSE)
  }
  if (any(v < 0)) {
    stop(sprintf("`%s` contient des valeurs negatives.", arg), call. = FALSE)
  }
  if (any(abs(v - round(v)) > 1e-8)) {
    stop(sprintf("`%s` doit contenir des entiers (comptages).", arg), call. = FALSE)
  }
  invisible(x)
}

# Conversion robuste en Date : Date, POSIXt ou texte (ISO, jj/mm/aaaa, ...)
#' @noRd
.as_date_robuste <- function(x, format = NULL, arg = "date_col") {
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
      "Impossible de convertir `%s` en dates. Precisez `format_date` (ex. \"%%d/%%m/%%Y\").",
      arg), call. = FALSE)
  }
  stop(sprintf("`%s` doit etre de classe Date, POSIXt ou texte.", arg), call. = FALSE)
}

# Debut de la periode contenant chaque date
#' @noRd
.debut_periode <- function(d, periode) {
  switch(periode,
    jour = d,
    semaine = d - (as.integer(format(d, "%u")) - 1L),
    mois = as.Date(format(d, "%Y-%m-01")),
    trimestre = {
      m <- as.integer(format(d, "%m"))
      as.Date(sprintf("%s-%02d-01", format(d, "%Y"), 3L * ((m - 1L) %/% 3L) + 1L))
    },
    annee = as.Date(paste0(format(d, "%Y"), "-01-01"))
  )
}

#' @noRd
.pas_periode <- function(periode) {
  c(jour = "day", semaine = "week", mois = "month",
    trimestre = "quarter", annee = "year")[[periode]]
}
