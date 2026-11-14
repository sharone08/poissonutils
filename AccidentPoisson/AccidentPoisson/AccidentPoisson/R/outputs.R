#' Visualiser des comptages
#'
#' @param data Un data.frame (ex. resultat de [analyser_zones()] ou
#'   [aggreger_accidents()]).
#' @param x_col Colonne de l'axe x.
#' @param y_col Colonne numerique de l'axe y.
#' @param type `"barres"`, `"ligne"` ou `"points"`.
#' @param group_col Colonne de groupe (couleur), ou `NULL`.
#' @param titre,xlab,ylab Titre et libelles des axes.
#' @param ordonner Si `TRUE`, trie les barres par valeur decroissante (barres
#'   sans groupe, axe x categoriel).
#' @param horizontal Si `TRUE`, barres horizontales.
#'
#' @return Un objet `ggplot`.
#' @export
#' @examples
#' z <- data.frame(zone = c("A", "B", "C"), total_accidents = c(12, 30, 7))
#' visualiser_comptages(z, "zone", "total_accidents", titre = "Accidents par zone")
visualiser_comptages <- function(data, x_col, y_col,
                                 type = c("barres", "ligne", "points"),
                                 group_col = NULL, titre = NULL,
                                 xlab = x_col, ylab = y_col,
                                 ordonner = TRUE, horizontal = FALSE) {
  type <- match.arg(type)
  .verifier_dataframe(data)
  .verifier_scalaire_chr(x_col, "x_col")
  .verifier_scalaire_chr(y_col, "y_col")
  .verifier_colonnes(data, c(x_col, y_col, group_col), "x_col/y_col/group_col")
  if (!is.numeric(data[[y_col]])) {
    stop("`y_col` doit etre une colonne numerique.", call. = FALSE)
  }
  d <- data
  x <- d[[x_col]]
  if (type == "barres" && isTRUE(ordonner) && is.null(group_col) &&
      !is.numeric(x) && !inherits(x, c("Date", "POSIXt"))) {
    d[[x_col]] <- stats::reorder(factor(x), -d[[y_col]], FUN = sum)
  }
  couleur <- "#1F78B4"
  if (is.null(group_col)) {
    mapping <- ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])
  } else {
    mapping <- ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]],
                            colour = .data[[group_col]],
                            fill = .data[[group_col]],
                            group = .data[[group_col]])
  }
  p <- ggplot2::ggplot(d, mapping)
  couche <- switch(type,
    barres = if (is.null(group_col)) ggplot2::geom_col(fill = couleur) else
      ggplot2::geom_col(position = "dodge"),
    ligne = if (is.null(group_col)) {
      list(ggplot2::geom_line(colour = couleur), ggplot2::geom_point(colour = couleur))
    } else {
      list(ggplot2::geom_line(), ggplot2::geom_point())
    },
    points = if (is.null(group_col)) ggplot2::geom_point(colour = couleur, size = 2.5) else
      ggplot2::geom_point(size = 2.5)
  )
  p <- p + couche +
    ggplot2::labs(title = titre, x = xlab, y = ylab) +
    ggplot2::theme_minimal()
  if (isTRUE(horizontal)) p <- p + ggplot2::coord_flip()
  p
}

#' Seuil d'alerte issu de la loi de Poisson
#'
#' Plus petit comptage consideré comme anormalement eleve : un comptage `x`
#' est inhabituel si \eqn{P(X \ge x) \le 1 - niveau} sous Poisson(lambda).
#'
#' @param lambda Intensite moyenne attendue (numerique `>= 0`).
#' @param niveau Niveau de probabilite (defaut `0.95`).
#'
#' @return Un vecteur numerique de seuils.
#' @export
#' @examples
#' calculer_seuil_alerte(3, 0.95)
calculer_seuil_alerte <- function(lambda, niveau = 0.95) {
  .verifier_lambda(lambda)
  .verifier_niveau(niveau)
  stats::qpois(niveau, lambda) + 1
}

#' Generer des alertes preventives
#'
#' @param data Un data.frame.
#' @param count_col Colonne de comptage.
#' @param seuil Seuil de vigilance renforcee (`x >= seuil`).
#' @param seuil_critique Seuil optionnel d'alerte critique (`>= seuil`).
#'
#' @return `data` avec les colonnes `niveau_vigilance` et `recommandation`.
#' @export
#' @examples
#' z <- data.frame(zone = c("A", "B", "C"), total_accidents = c(5, 12, 20))
#' generer_alerte(z, "total_accidents", seuil = 10, seuil_critique = 15)
generer_alerte <- function(data, count_col, seuil, seuil_critique = NULL) {
  .verifier_dataframe(data)
  .verifier_scalaire_chr(count_col, "count_col")
  .verifier_colonnes(data, count_col, "count_col")
  x <- data[[count_col]]
  if (!is.numeric(x)) stop("`count_col` doit etre numerique.", call. = FALSE)
  if (!is.numeric(seuil) || length(seuil) != 1L || is.na(seuil)) {
    stop("`seuil` doit etre un nombre.", call. = FALSE)
  }
  if (!is.null(seuil_critique)) {
    if (!is.numeric(seuil_critique) || length(seuil_critique) != 1L ||
        is.na(seuil_critique) || seuil_critique < seuil) {
      stop("`seuil_critique` doit etre un nombre superieur ou egal a `seuil`.",
           call. = FALSE)
    }
  }
  niveau <- ifelse(x >= seuil, "Vigilance renforcee", "Surveillance habituelle")
  reco <- ifelse(x >= seuil,
                 "Examiner les circonstances et renforcer la surveillance",
                 "Maintenir la surveillance")
  if (!is.null(seuil_critique)) {
    crit <- !is.na(x) & x >= seuil_critique
    niveau[crit] <- "Alerte critique"
    reco[crit] <- "Intervention prioritaire : analyser les causes et agir sans delai"
  }
  manquant <- is.na(x)
  niveau[manquant] <- "Donnee manquante"
  reco[manquant] <- "Verifier la donnee"
  data$niveau_vigilance <- niveau
  data$recommandation <- reco
  data
}

#' Exporter des resultats
#'
#' @param data Un data.frame, ou une liste nommee de data.frames (une feuille
#'   par element pour `xlsx`).
#' @param fichier Chemin du fichier ; le format est deduit de l'extension :
#'   `.csv`, `.xlsx` (package `writexl`) ou `.rds`.
#' @param ecraser Si `FALSE`, refuse d'ecraser un fichier existant.
#' @param format_fr Pour le CSV : separateur `;` et virgule decimale
#'   (Excel francais).
#'
#' @return Le chemin du fichier ecrit (invisible).
#' @export
#' @examples
#' f <- tempfile(fileext = ".csv")
#' exporter_resultats(data.frame(a = 1:3), f)
exporter_resultats <- function(data, fichier, ecraser = TRUE, format_fr = FALSE) {
  if (!is.character(fichier) || length(fichier) != 1L || is.na(fichier)) {
    stop("`fichier` doit etre un chemin (chaine de caracteres).", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(fichier))
  if (!ext %in% c("csv", "xlsx", "rds")) {
    stop("Format non supporte : utilisez .csv, .xlsx ou .rds.", call. = FALSE)
  }
  if (is.data.frame(data)) {
    liste <- list(Resultats = data)
  } else if (is.list(data) && length(data) > 0L &&
             all(vapply(data, is.data.frame, logical(1)))) {
    liste <- data
    if (is.null(names(liste)) || any(!nzchar(names(liste)))) {
      names(liste) <- paste0("Feuille", seq_along(liste))
    }
  } else {
    stop("`data` doit etre un data.frame ou une liste de data.frames.",
         call. = FALSE)
  }
  if (file.exists(fichier) && !isTRUE(ecraser)) {
    stop("Le fichier existe deja (utilisez ecraser = TRUE).", call. = FALSE)
  }
  dossier <- dirname(fichier)
  if (!dir.exists(dossier)) dir.create(dossier, recursive = TRUE)

  if (ext == "csv") {
    if (length(liste) > 1L) {
      stop("Le format CSV n'accepte qu'un seul tableau ; utilisez .xlsx ou .rds.",
           call. = FALSE)
    }
    ecrire <- if (isTRUE(format_fr)) utils::write.csv2 else utils::write.csv
    ecrire(liste[[1L]], file = fichier, row.names = FALSE, fileEncoding = "UTF-8")
  } else if (ext == "xlsx") {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop("Le package 'writexl' est requis pour l'export xlsx : install.packages(\"writexl\").",
           call. = FALSE)
    }
    writexl::write_xlsx(liste, path = fichier)
  } else {
    saveRDS(if (is.data.frame(data)) data else liste, fichier)
  }
  invisible(normalizePath(fichier))
}

#' Charger le jeu de donnees de demonstration (RN3)
#'
#' Jeu **synthetique** (1096 jours, 2022-2024) fourni uniquement pour tester
#' le package ; il ne represente pas des accidents reels.
#'
#' @return Un data.frame avec `Date` (classe Date), `Annee`, `Mois`,
#'   `Jour_Semaine`, `Troncon`, `Meteo`, `Trafic`, `Nb_Accidents`,
#'   `Nb_Blesses`, `Nb_Deces`, `Gravite_Moyenne`.
#' @export
#' @examples
#' d <- charger_exemple_rn3()
#' head(d)
charger_exemple_rn3 <- function() {
  chemin <- system.file("extdata", "accidents_rn3_demo.csv",
                        package = "AccidentPoisson")
  if (!nzchar(chemin)) {
    stop("Fichier d'exemple introuvable : reinstallez le package.", call. = FALSE)
  }
  d <- utils::read.csv(chemin, stringsAsFactors = FALSE, encoding = "UTF-8")
  d$Date <- as.Date(d$Date)
  d
}
