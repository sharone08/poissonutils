#' Agreger des accidents par periode (et par groupe)
#'
#' Transforme des donnees d'accidents (une ligne par accident, ou une ligne
#' par observation avec un comptage) en comptages par periode et par groupe,
#' prets pour la regression de Poisson.
#'
#' @param data Un data.frame.
#' @param date_col Nom de la colonne de date (Date, POSIXt ou texte au format
#'   `aaaa-mm-jj`, `jj/mm/aaaa`, ...).
#' @param group_cols Noms de colonnes de regroupement (ex. troncon, ville),
#'   ou `NULL`.
#' @param periode Granularite : `"jour"`, `"semaine"` (debut le lundi),
#'   `"mois"`, `"trimestre"` ou `"annee"`.
#' @param count_col Colonne de comptage. Si `NULL`, chaque ligne est un
#'   accident.
#' @param completer Si `TRUE`, ajoute les combinaisons periode x groupe
#'   absentes avec un comptage de `0`. Par defaut `TRUE` quand `count_col` est
#'   `NULL` (une absence de ligne signifie zero accident) et `FALSE` sinon.
#' @param format_date Format de date explicite (ex. `"%d/%m/%Y"`), sinon
#'   detection automatique.
#'
#' @return Un data.frame avec `periode` (date de debut), les colonnes de
#'   groupe, `nb_accidents`, `nb_observations` (uniquement si `count_col` est
#'   fourni : nombre de lignes agregees) et `nb_jours` (jours de la periode
#'   couverts par les donnees, utilisable comme exposition). Les lignes dont
#'   la date, le comptage ou un groupe est manquant sont ignorees.
#' @export
#' @examples
#' ev <- data.frame(
#'   date = c("2025-01-03", "2025-01-15", "2025-03-02", "2025-01-20"),
#'   zone = c("A", "A", "B", "A")
#' )
#' aggreger_accidents(ev, "date", "zone", "mois")
aggreger_accidents <- function(data, date_col, group_cols = NULL,
                               periode = c("mois", "jour", "semaine",
                                           "trimestre", "annee"),
                               count_col = NULL,
                               completer = is.null(count_col),
                               format_date = NULL) {
  periode <- match.arg(periode)
  .verifier_dataframe(data)
  .verifier_scalaire_chr(date_col, "date_col")
  .verifier_colonnes(data, date_col, "date_col")
  if (!is.null(group_cols)) .verifier_colonnes(data, group_cols, "group_cols")
  if (!is.null(count_col)) {
    .verifier_scalaire_chr(count_col, "count_col")
    .verifier_colonnes(data, count_col, "count_col")
  }
  reserves <- c("periode", "nb_accidents", "nb_observations", "nb_jours")
  if (any(group_cols %in% reserves)) {
    stop("Les noms de colonnes de groupe ne peuvent pas etre : ",
         paste(reserves, collapse = ", "), ".", call. = FALSE)
  }

  dates <- .as_date_robuste(data[[date_col]], format_date, date_col)
  if (is.null(count_col)) {
    cnt <- rep(1, nrow(data))
  } else {
    cnt <- data[[count_col]]
    .verifier_comptage(cnt, count_col)
  }
  garder <- !is.na(dates) & !is.na(cnt)
  if (any(!garder)) {
    warning(sprintf("%d ligne(s) ignoree(s) (date ou comptage manquant).",
                    sum(!garder)), call. = FALSE)
  }
  if (!any(garder)) stop("Aucune ligne exploitable.", call. = FALSE)

  df <- data.frame(periode = .debut_periode(dates, periode),
                   nb_accidents = cnt, stringsAsFactors = FALSE)
  if (!is.null(count_col)) df$nb_observations <- 1
  for (g in group_cols) df[[g]] <- data[[g]]
  df <- df[garder, , drop = FALSE]

  d_min <- min(dates[garder])
  d_max <- max(dates[garder])
  valeurs <- setdiff(names(df), c("periode", group_cols))
  agg <- stats::aggregate(df[valeurs], by = df[c("periode", group_cols)],
                          FUN = sum)

  pas <- .pas_periode(periode)
  periodes <- seq(.debut_periode(d_min, periode),
                  .debut_periode(d_max, periode), by = pas)

  if (isTRUE(completer)) {
    grille <- data.frame(periode = periodes)
    if (length(group_cols) > 0L) {
      grille <- merge(grille, unique(agg[group_cols]), by = NULL)
    }
    res <- merge(grille, agg, by = c("periode", group_cols), all.x = TRUE)
    for (v in valeurs) res[[v]][is.na(res[[v]])] <- 0
  } else {
    res <- agg
  }

  # Nombre de jours de chaque periode couverts par les donnees
  suivant <- seq(periodes[length(periodes)], by = pas, length.out = 2L)[2L]
  fins <- c(periodes[-1L], suivant) - 1L
  idx <- match(res$periode, periodes)
  res$nb_jours <- as.numeric(pmin(fins[idx], d_max) - pmax(periodes[idx], d_min)) + 1

  res <- res[do.call(order, unname(as.list(res[c(group_cols, "periode")]))), , drop = FALSE]
  rownames(res) <- NULL
  attr(res, "periode") <- periode
  res
}
