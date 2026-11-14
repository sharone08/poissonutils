#' Ajuster un modele de regression de Poisson
#'
#' Ajuste un modele lineaire generalise de famille Poisson (lien log), avec
#' en option une variable d'exposition (offset `log(exposition)`).
#'
#' @param data Un data.frame **agrege** : la reponse est un comptage par
#'   periode et/ou par zone (voir [aggreger_accidents()]), pas une ligne par
#'   accident.
#' @param formule Une formule (ou un texte), ex. `nb_accidents ~ pluie + vitesse`.
#' @param exposition_col Nom d'une colonne d'exposition (trafic, nombre de
#'   jours...), strictement positive, ou `NULL`. Ne pas la mettre aussi dans
#'   la formule.
#' @param famille `"poisson"` (defaut) ou `"quasipoisson"` (corrige les
#'   erreurs standard en cas de surdispersion).
#'
#' @return Un objet `glm`. Le nom de la colonne d'exposition est en attribut
#'   `exposition_col`.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(pluie = rbinom(60, 1, 0.4), trafic = runif(60, 50, 150))
#' d$y <- rpois(60, exp(-3 + 0.5 * d$pluie) * d$trafic)
#' m <- ajuster_modele_poisson(d, y ~ pluie, exposition_col = "trafic")
#' summary(m)
ajuster_modele_poisson <- function(data, formule, exposition_col = NULL,
                                   famille = c("poisson", "quasipoisson")) {
  famille <- match.arg(famille)
  .verifier_dataframe(data)
  if (is.character(formule) && length(formule) == 1L) {
    formule <- stats::as.formula(formule)
  }
  if (!inherits(formule, "formula") || length(formule) != 3L) {
    stop("`formule` doit etre une formule avec une reponse, ex. nb ~ pluie.",
         call. = FALSE)
  }
  vars <- setdiff(all.vars(formule), ".")
  manquantes <- setdiff(vars, names(data))
  if (length(manquantes) > 0L) {
    stop("Variable(s) de la formule absente(s) de `data` : ",
         paste(manquantes, collapse = ", "), ".", call. = FALSE)
  }
  y <- eval(formule[[2L]], data, environment(formule))
  if (!is.numeric(y)) stop("La reponse doit etre un comptage numerique.", call. = FALSE)
  .verifier_comptage(y, "reponse")

  if (!is.null(exposition_col)) {
    .verifier_scalaire_chr(exposition_col, "exposition_col")
    .verifier_colonnes(data, exposition_col, "exposition_col")
    e <- data[[exposition_col]]
    if (!is.numeric(e) || anyNA(e) || any(e <= 0)) {
      stop("`exposition_col` doit etre numerique, sans valeur manquante et strictement positive.",
           call. = FALSE)
    }
    formule <- stats::update.formula(
      formule,
      stats::as.formula(paste0(". ~ . + offset(log(`", exposition_col, "`))"))
    )
  }

  fam <- if (famille == "poisson") stats::poisson(link = "log") else
    stats::quasipoisson(link = "log")
  modele <- stats::glm(formula = formule, data = data, family = fam)
  if (!isTRUE(modele$converged)) {
    warning("L'algorithme n'a pas converge : resultats a verifier.", call. = FALSE)
  }
  modele$call <- call("glm", formula = formule, family = as.name(famille),
                      data = as.name("data"))
  attr(modele, "exposition_col") <- exposition_col
  modele
}

#' Predire des nombres d'accidents
#'
#' @param modele Un modele ajuste par [ajuster_modele_poisson()].
#' @param nouvelles_donnees Un data.frame contenant les variables du modele
#'   (et la colonne d'exposition si le modele en utilise une). Si `NULL`,
#'   predictions sur les donnees d'ajustement (sans les covariables).
#' @param niveau Niveau de confiance de l'intervalle sur l'esperance.
#'
#' @return Un data.frame : les `nouvelles_donnees` (si fournies) puis
#'   `prediction` (nombre attendu d'accidents), `ic_inf`, `ic_sup` et
#'   `p_au_moins_un` (probabilite d'au moins un accident, `1 - exp(-prediction)`).
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(pluie = rbinom(60, 1, 0.4), trafic = runif(60, 50, 150))
#' d$y <- rpois(60, exp(-3 + 0.5 * d$pluie) * d$trafic)
#' m <- ajuster_modele_poisson(d, y ~ pluie, exposition_col = "trafic")
#' predire_accidents(m, data.frame(pluie = c(0, 1), trafic = 100))
predire_accidents <- function(modele, nouvelles_donnees = NULL, niveau = 0.95) {
  if (!inherits(modele, "glm")) {
    stop("`modele` doit etre un objet glm (voir ajuster_modele_poisson()).",
         call. = FALSE)
  }
  .verifier_niveau(niveau)
  if (!is.null(nouvelles_donnees)) {
    .verifier_dataframe(nouvelles_donnees, "nouvelles_donnees")
    expo <- attr(modele, "exposition_col")
    if (!is.null(expo) && !expo %in% names(nouvelles_donnees)) {
      stop(sprintf("`nouvelles_donnees` doit contenir la colonne d'exposition `%s`.",
                   expo), call. = FALSE)
    }
  }
  pr <- stats::predict(modele, newdata = nouvelles_donnees, type = "link",
                       se.fit = TRUE)
  z <- stats::qnorm(1 - (1 - niveau) / 2)
  fit <- as.numeric(pr$fit)
  se <- as.numeric(pr$se.fit)
  out <- data.frame(
    prediction = exp(fit),
    ic_inf = exp(fit - z * se),
    ic_sup = exp(fit + z * se),
    p_au_moins_un = 1 - exp(-exp(fit))
  )
  if (!is.null(nouvelles_donnees)) {
    out <- cbind(nouvelles_donnees, out)
  }
  rownames(out) <- NULL
  out
}

#' Diagnostiquer la surdispersion d'un modele de Poisson
#'
#' Calcule le rapport Pearson X2 / ddl (proche de 1 si Poisson convient), le
#' test du khi-deux de Pearson et le test de regression de Cameron & Trivedi
#' (1990) de l'hypothese Var = (1 + alpha) * moyenne contre alpha > 0.
#'
#' @param modele Un modele `glm` de famille poisson ou quasipoisson.
#' @param niveau Seuil de decision (defaut `0.05`).
#'
#' @return Un objet `diagnostic_surdispersion` (liste) : `dispersion`,
#'   `chi2`, `df`, `p_pearson`, `alpha`, `p_cameron_trivedi`, `conclusion`,
#'   `recommandation`.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(pluie = rbinom(80, 1, 0.4))
#' d$y <- rpois(80, exp(1 + 0.3 * d$pluie))
#' diagnostiquer_surdispersion(ajuster_modele_poisson(d, y ~ pluie))
diagnostiquer_surdispersion <- function(modele, niveau = 0.05) {
  if (!inherits(modele, "glm") ||
      !modele$family$family %in% c("poisson", "quasipoisson")) {
    stop("`modele` doit etre un glm de famille poisson ou quasipoisson.",
         call. = FALSE)
  }
  .verifier_niveau(niveau)
  ddl <- modele$df.residual
  if (ddl < 1L) stop("Pas assez de degres de liberte residuels.", call. = FALSE)

  chi2 <- sum(stats::residuals(modele, type = "pearson")^2)
  phi <- chi2 / ddl
  p_sur <- stats::pchisq(chi2, ddl, lower.tail = FALSE)
  p_sous <- stats::pchisq(chi2, ddl)

  y <- modele$y
  mu <- stats::fitted(modele)
  aux <- ((y - mu)^2 - y) / mu
  aux <- aux[is.finite(aux)]
  alpha <- NA_real_
  p_ct <- NA_real_
  if (length(aux) >= 3L) {
    ajust <- stats::lm(aux ~ 1)
    coefs <- summary(ajust)$coefficients
    alpha <- coefs[1L, 1L]
    p_ct <- stats::pt(coefs[1L, 3L], df = ajust$df.residual, lower.tail = FALSE)
  }

  if (p_sur < niveau && phi > 1) {
    conclusion <- "Surdispersion detectee"
    reco <- "Utiliser famille = 'quasipoisson' ou un modele binomial negatif (MASS::glm.nb)."
  } else if (p_sous < niveau && phi < 1) {
    conclusion <- "Sous-dispersion detectee"
    reco <- "Verifier la structure des donnees ; un modele quasi-Poisson peut convenir."
  } else {
    conclusion <- "Pas de signe de sur- ou sous-dispersion"
    reco <- "Le modele de Poisson est coherent avec la dispersion observee."
  }
  structure(
    list(dispersion = phi, chi2 = chi2, df = ddl, p_pearson = p_sur,
         alpha = alpha, p_cameron_trivedi = p_ct,
         conclusion = conclusion, recommandation = reco),
    class = "diagnostic_surdispersion"
  )
}

#' @export
print.diagnostic_surdispersion <- function(x, ...) {
  cat("Diagnostic de surdispersion (modele de Poisson)\n")
  cat(sprintf("Dispersion de Pearson (X2/ddl) : %.3f  [X2 = %.2f, ddl = %d]\n",
              x$dispersion, x$chi2, as.integer(x$df)))
  cat(sprintf("p-value (Pearson, surdispersion)      : %s\n",
              format.pval(x$p_pearson, digits = 4)))
  cat(sprintf("alpha (Cameron-Trivedi) : %.3f | p-value : %s\n",
              x$alpha, format.pval(x$p_cameron_trivedi, digits = 4)))
  cat("Conclusion    :", x$conclusion, "\n")
  cat("Recommandation:", x$recommandation, "\n")
  invisible(x)
}
