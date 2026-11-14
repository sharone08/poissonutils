#' Analyser les zones dangereuses
#'
#' Classe les zones (troncons, villes, ...) selon leur taux d'accidents et
#' teste, pour chacune, si ce taux s'ecarte du taux global (test exact de
#' Poisson bilateral, avec correction des comparaisons multiples).
#'
#' L'exposition de chaque zone est : la somme de `exposition_col` si fournie ;
#' sinon le nombre de lignes de la zone si `count_col` est fourni (une ligne =
#' une unite d'observation, ex. un jour) ; sinon (une ligne = un accident) les
#' zones sont supposees d'exposition egale.
#'
#' @param data Un data.frame.
#' @param zone_col Nom de la colonne identifiant la zone.
#' @param count_col Colonne de comptage. Si `NULL`, chaque ligne est un accident.
#' @param exposition_col Colonne d'exposition (trafic, longueur, jours...),
#'   numerique strictement positive, ou `NULL`.
#' @param niveau Seuil de decision apres ajustement (defaut `0.05`).
#' @param ajustement Methode de `stats::p.adjust()` (defaut `"holm"`).
#'
#' @return Un data.frame trie par rang : `zone`, `nb_observations`,
#'   `total_accidents`, `part_accidents`, `exposition`, `taux`,
#'   `ratio_taux` (taux / taux global), `p_value`, `p_ajustee`, `rang`,
#'   `score_risque` (0-10, relatif a la zone la plus exposee) et
#'   `classification` (`"Risque eleve"`, `"Risque faible"` ou
#'   `"Dans la moyenne"`). Le taux global est en attribut `taux_global`.
#' @export
#' @examples
#' d <- data.frame(ville = rep(c("A", "B", "C"), each = 50),
#'                 nombre = c(rep(1, 50), rep(1, 50), rep(4, 50)))
#' analyser_zones(d, zone_col = "ville", count_col = "nombre")
analyser_zones <- function(data, zone_col, count_col = NULL,
                           exposition_col = NULL, niveau = 0.05,
                           ajustement = "holm") {
  .verifier_dataframe(data)
  .verifier_scalaire_chr(zone_col, "zone_col")
  .verifier_colonnes(data, zone_col, "zone_col")
  .verifier_niveau(niveau)
  if (!is.character(ajustement) || length(ajustement) != 1L ||
      !ajustement %in% stats::p.adjust.methods) {
    stop("`ajustement` doit etre une methode de stats::p.adjust().", call. = FALSE)
  }

  zone <- data[[zone_col]]
  if (is.null(count_col)) {
    cnt <- rep(1, nrow(data))
  } else {
    .verifier_scalaire_chr(count_col, "count_col")
    .verifier_colonnes(data, count_col, "count_col")
    cnt <- data[[count_col]]
    .verifier_comptage(cnt, count_col)
  }
  if (is.null(exposition_col)) {
    e <- rep(1, nrow(data))
  } else {
    .verifier_scalaire_chr(exposition_col, "exposition_col")
    .verifier_colonnes(data, exposition_col, "exposition_col")
    e <- data[[exposition_col]]
    if (!is.numeric(e) || any(e[!is.na(e)] <= 0)) {
      stop("`exposition_col` doit etre numerique et strictement positive.",
           call. = FALSE)
    }
  }

  garder <- !is.na(zone) & !is.na(cnt) & !is.na(e)
  if (any(!garder)) {
    warning(sprintf("%d ligne(s) ignoree(s) (zone, comptage ou exposition manquant).",
                    sum(!garder)), call. = FALSE)
  }
  if (!any(garder)) stop("Aucune ligne exploitable.", call. = FALSE)
  zone <- droplevels(as.factor(zone[garder]))
  cnt <- cnt[garder]
  e <- e[garder]

  total <- as.numeric(tapply(cnt, zone, sum))
  nb_obs <- as.integer(table(zone))
  if (sum(total) == 0) stop("Aucun accident dans les donnees.", call. = FALSE)
  expo <- if (!is.null(exposition_col)) {
    as.numeric(tapply(e, zone, sum))
  } else if (!is.null(count_col)) {
    as.numeric(nb_obs)
  } else {
    rep(1, length(total))
  }

  taux_global <- sum(total) / sum(expo)
  taux <- total / expo
  attendu <- taux_global * expo
  p_haut <- stats::ppois(total - 1, attendu, lower.tail = FALSE)
  p_bas <- stats::ppois(total, attendu)
  p <- pmin(1, 2 * pmin(p_haut, p_bas))
  p_adj <- stats::p.adjust(p, method = ajustement)

  classification <- ifelse(
    p_adj < niveau & taux > taux_global, "Risque eleve",
    ifelse(p_adj < niveau & taux < taux_global, "Risque faible",
           "Dans la moyenne")
  )
  res <- data.frame(
    zone = levels(zone),
    nb_observations = nb_obs,
    total_accidents = total,
    part_accidents = total / sum(total),
    exposition = expo,
    taux = taux,
    ratio_taux = taux / taux_global,
    p_value = p,
    p_ajustee = p_adj,
    rang = rank(-taux, ties.method = "min"),
    score_risque = taux / max(taux) * 10,
    classification = classification,
    stringsAsFactors = FALSE
  )
  res <- res[order(res$rang, res$zone), , drop = FALSE]
  rownames(res) <- NULL
  attr(res, "taux_global") <- taux_global
  res
}
