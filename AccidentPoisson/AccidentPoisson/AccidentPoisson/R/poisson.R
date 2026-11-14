#' Probabilites de la loi de Poisson
#'
#' Calcule \eqn{P(X = k)}, \eqn{P(X \le k)} ou \eqn{P(X > k)} pour une
#' variable \eqn{X \sim \mathrm{Poisson}(\lambda)}.
#'
#' @param lambda Intensite moyenne (nombre moyen d'accidents par unite de
#'   temps), numerique `>= 0`.
#' @param k Nombre(s) d'accidents (entiers `>= 0`).
#' @param cumul Si `TRUE`, retourne \eqn{P(X \le k)}.
#' @param superieur Si `TRUE`, retourne \eqn{P(X > k)} (probabilite de
#'   depasser `k`). Incompatible avec `cumul = TRUE`.
#'
#' @return Un vecteur numerique de probabilites.
#' @export
#' @examples
#' calculer_poisson(3, 2)                    # P(X = 2)
#' calculer_poisson(3, 2, cumul = TRUE)      # P(X <= 2)
#' calculer_poisson(3, 5, superieur = TRUE)  # P(X > 5)
calculer_poisson <- function(lambda, k, cumul = FALSE, superieur = FALSE) {
  .verifier_lambda(lambda)
  if (!is.numeric(k) || length(k) == 0L || anyNA(k) || any(k < 0) ||
      any(abs(k - round(k)) > 1e-8)) {
    stop("`k` doit contenir des entiers positifs ou nuls.", call. = FALSE)
  }
  if (isTRUE(cumul) && isTRUE(superieur)) {
    stop("`cumul` et `superieur` ne peuvent pas etre TRUE en meme temps.",
         call. = FALSE)
  }
  if (isTRUE(superieur)) {
    stats::ppois(k, lambda, lower.tail = FALSE)
  } else if (isTRUE(cumul)) {
    stats::ppois(k, lambda)
  } else {
    stats::dpois(k, lambda)
  }
}

#' Tableau des probabilites de Poisson
#'
#' @param lambda Intensite moyenne (un seul nombre `>= 0`).
#' @param k_max Plus grande valeur de `k` du tableau (entier `>= 0`).
#' @param jours Nombre de jours de la periode (ex. `365`). S'il est fourni,
#'   la colonne `jours_attendus` donne le nombre de jours attendus avec
#'   exactement `k` accidents.
#'
#' @return Un data.frame avec les colonnes `k`, `p_egal` (P(X = k)),
#'   `p_cumulee` (P(X <= k)), `p_superieure` (P(X > k)) et
#'   eventuellement `jours_attendus`.
#' @export
#' @examples
#' table_poisson(lambda = 3, k_max = 10)
#' table_poisson(lambda = 3, k_max = 10, jours = 365)
table_poisson <- function(lambda, k_max, jours = NULL) {
  .verifier_lambda(lambda)
  if (length(lambda) != 1L) stop("`lambda` doit etre un seul nombre.", call. = FALSE)
  if (!is.numeric(k_max) || length(k_max) != 1L || is.na(k_max) || k_max < 0 ||
      abs(k_max - round(k_max)) > 1e-8) {
    stop("`k_max` doit etre un entier positif ou nul.", call. = FALSE)
  }
  k <- 0:as.integer(round(k_max))
  res <- data.frame(
    k = k,
    p_egal = stats::dpois(k, lambda),
    p_cumulee = stats::ppois(k, lambda),
    p_superieure = stats::ppois(k, lambda, lower.tail = FALSE)
  )
  if (!is.null(jours)) {
    if (!is.numeric(jours) || length(jours) != 1L || is.na(jours) || jours <= 0) {
      stop("`jours` doit etre un nombre strictement positif.", call. = FALSE)
    }
    res$jours_attendus <- res$p_egal * jours
  }
  res
}

#' Estimation de lambda avec intervalle de confiance exact
#'
#' Estime l'intensite de Poisson par la moyenne empirique et fournit un
#' intervalle de confiance exact (Garwood), ainsi que l'indice de dispersion
#' variance / moyenne (proche de 1 pour des donnees de Poisson).
#'
#' @param x Vecteur de comptages (entiers `>= 0`). Les `NA` sont ignores.
#' @param niveau Niveau de confiance (defaut `0.95`).
#'
#' @return Un data.frame d'une ligne : `n`, `total`, `lambda`, `variance`,
#'   `indice_dispersion`, `ic_inf`, `ic_sup`.
#' @export
#' @examples
#' estimer_lambda(c(2, 3, 4, 3, 5, 1, 2))
estimer_lambda <- function(x, niveau = 0.95) {
  .verifier_comptage(x, "x")
  .verifier_niveau(niveau)
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2L) stop("Au moins 2 observations sont necessaires.", call. = FALSE)
  s <- sum(x)
  a <- 1 - niveau
  inf <- if (s == 0) 0 else stats::qchisq(a / 2, 2 * s) / (2 * n)
  sup <- stats::qchisq(1 - a / 2, 2 * (s + 1)) / (2 * n)
  m <- mean(x)
  v <- stats::var(x)
  data.frame(
    n = n, total = s, lambda = m, variance = v,
    indice_dispersion = if (m > 0) v / m else NA_real_,
    ic_inf = inf, ic_sup = sup
  )
}

#' Test d'adequation du khi-deux a la loi de Poisson
#'
#' Compare la distribution observee des comptages a la distribution de
#' Poisson de parametre estime. Les classes a effectif theorique inferieur a
#' `min_attendu` sont regroupees (en queue puis en tete). Le nombre de
#' degres de liberte est `classes - 2` (un parametre estime).
#'
#' @param x Vecteur de comptages (au moins 10 observations).
#' @param min_attendu Effectif theorique minimal par classe (defaut `5`).
#' @param niveau Seuil de decision (defaut `0.05`).
#'
#' @return Un objet de classe `test_poisson` (liste) avec `table`, `lambda`,
#'   `n`, `statistic`, `df`, `p_value` et `conclusion`.
#' @export
#' @examples
#' x <- rep(0:8, times = round(1000 * dpois(0:8, 3)))
#' tester_adequation_poisson(x)
tester_adequation_poisson <- function(x, min_attendu = 5, niveau = 0.05) {
  .verifier_comptage(x, "x")
  .verifier_niveau(niveau)
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 10L) stop("Au moins 10 observations sont necessaires.", call. = FALSE)
  lambda <- mean(x)
  if (lambda <= 0) {
    stop("Toutes les observations sont nulles : test impossible.", call. = FALSE)
  }
  kmax <- as.integer(round(max(x)))
  k <- 0:kmax
  obs <- as.numeric(tabulate(as.integer(round(x)) + 1L, nbins = kmax + 1L))
  p <- stats::dpois(k, lambda)
  p[kmax + 1L] <- stats::ppois(kmax - 1L, lambda, lower.tail = FALSE)
  attendu <- n * p
  lo <- k
  hi <- k
  hi[kmax + 1L] <- Inf

  while (length(attendu) > 2L && attendu[length(attendu)] < min_attendu) {
    m <- length(attendu)
    obs[m - 1L] <- obs[m - 1L] + obs[m]
    attendu[m - 1L] <- attendu[m - 1L] + attendu[m]
    hi[m - 1L] <- hi[m]
    obs <- obs[-m]; attendu <- attendu[-m]; lo <- lo[-m]; hi <- hi[-m]
  }
  while (length(attendu) > 2L && attendu[1L] < min_attendu) {
    obs[2L] <- obs[1L] + obs[2L]
    attendu[2L] <- attendu[1L] + attendu[2L]
    lo[2L] <- lo[1L]
    obs <- obs[-1L]; attendu <- attendu[-1L]; lo <- lo[-1L]; hi <- hi[-1L]
  }

  contribution <- (obs - attendu)^2 / attendu
  stat <- sum(contribution)
  ddl <- length(attendu) - 2L
  if (ddl < 1L) {
    warning("Trop peu de classes apres regroupement : p-value non calculee.",
            call. = FALSE)
    pval <- NA_real_
  } else {
    pval <- stats::pchisq(stat, ddl, lower.tail = FALSE)
  }
  classe <- ifelse(is.infinite(hi), paste0(">=", lo),
                   ifelse(lo == hi, as.character(lo), paste0(lo, "-", hi)))
  conclusion <- if (is.na(pval)) {
    "Test non concluant"
  } else if (pval < niveau) {
    "Rejet de H0 : la distribution s'ecarte d'une loi de Poisson"
  } else {
    "Non-rejet de H0 : distribution compatible avec une loi de Poisson"
  }
  structure(
    list(
      table = data.frame(classe = classe, observe = obs, attendu = attendu,
                         contribution = contribution),
      lambda = lambda, n = n, statistic = stat, df = ddl,
      p_value = pval, conclusion = conclusion
    ),
    class = "test_poisson"
  )
}

#' @export
print.test_poisson <- function(x, ...) {
  cat("Test d'adequation a la loi de Poisson (khi-deux)\n")
  cat(sprintf("n = %d | lambda estime = %.4f\n", x$n, x$lambda))
  cat(sprintf("X2 = %.3f | ddl = %d | p-value = %s\n",
              x$statistic, x$df, format.pval(x$p_value, digits = 4)))
  cat("Conclusion :", x$conclusion, "\n\n")
  print(x$table, row.names = FALSE)
  invisible(x)
}
