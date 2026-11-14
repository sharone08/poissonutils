#' Fit a Poisson regression model
#'
#' Fits a generalized linear model with Poisson family and log link, with an
#' optional exposure variable (offset `log(exposure)`).
#'
#' @param data A data.frame of **aggregated** data: the response is a count per
#'   period and/or zone (see [aggregate_counts()]), not one row per event.
#' @param formula A formula (or a string), e.g. `count ~ rain + speed`.
#' @param exposure_col Name of an exposure column (traffic, number of days...),
#'   strictly positive, or `NULL`. Do not also put it in the formula.
#' @param family `"poisson"` (default) or `"quasipoisson"` (corrects standard
#'   errors under overdispersion).
#'
#' @return A `glm` object. The exposure column name is stored in attribute
#'   `exposure_col`.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(rain = rbinom(60, 1, 0.4), traffic = runif(60, 50, 150))
#' d$y <- rpois(60, exp(-3 + 0.5 * d$rain) * d$traffic)
#' m <- fit_poisson_model(d, y ~ rain, exposure_col = "traffic")
#' summary(m)
fit_poisson_model <- function(data, formula, exposure_col = NULL,
                              family = c("poisson", "quasipoisson")) {
  family <- match.arg(family)
  .check_df(data)
  if (is.character(formula) && length(formula) == 1L) {
    formula <- stats::as.formula(formula)
  }
  if (!inherits(formula, "formula") || length(formula) != 3L) {
    stop("`formula` must be a formula with a response, e.g. n ~ rain.",
         call. = FALSE)
  }
  vars <- setdiff(all.vars(formula), ".")
  absent <- setdiff(vars, names(data))
  if (length(absent) > 0L) {
    stop("Variable(s) in the formula not found in `data`: ",
         paste(absent, collapse = ", "), ".", call. = FALSE)
  }
  y <- eval(formula[[2L]], data, environment(formula))
  if (!is.numeric(y)) stop("The response must be a numeric count.", call. = FALSE)
  .check_counts(y, "response")

  if (!is.null(exposure_col)) {
    .check_single_col(exposure_col, "exposure_col")
    .check_cols(data, exposure_col, "exposure_col")
    e <- data[[exposure_col]]
    if (!is.numeric(e) || anyNA(e) || any(e <= 0)) {
      stop("`exposure_col` must be numeric, without missing values and strictly positive.",
           call. = FALSE)
    }
    formula <- stats::update.formula(
      formula,
      stats::as.formula(paste0(". ~ . + offset(log(`", exposure_col, "`))"))
    )
  }

  fam <- if (family == "poisson") stats::poisson(link = "log") else
    stats::quasipoisson(link = "log")
  model <- stats::glm(formula = formula, data = data, family = fam)
  if (!isTRUE(model$converged)) {
    warning("The algorithm did not converge: check the results.", call. = FALSE)
  }
  model$call <- call("glm", formula = formula, family = as.name(family),
                     data = as.name("data"))
  attr(model, "exposure_col") <- exposure_col
  model
}

#' Predict expected counts
#'
#' @param model A model fitted with [fit_poisson_model()].
#' @param newdata A data.frame with the model variables (and the exposure
#'   column if the model uses one). If `NULL`, predictions for the fitting
#'   data (without covariates).
#' @param level Confidence level of the interval on the expected count.
#'
#' @return A data.frame: `newdata` (if given) followed by `prediction`
#'   (expected count), `ci_lower`, `ci_upper` and `prob_at_least_one`
#'   (probability of at least one event, `1 - exp(-prediction)`).
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(rain = rbinom(60, 1, 0.4), traffic = runif(60, 50, 150))
#' d$y <- rpois(60, exp(-3 + 0.5 * d$rain) * d$traffic)
#' m <- fit_poisson_model(d, y ~ rain, exposure_col = "traffic")
#' predict_counts(m, data.frame(rain = c(0, 1), traffic = 100))
predict_counts <- function(model, newdata = NULL, level = 0.95) {
  if (!inherits(model, "glm")) {
    stop("`model` must be a glm object (see fit_poisson_model()).", call. = FALSE)
  }
  .check_level(level)
  if (!is.null(newdata)) {
    .check_df(newdata, "newdata")
    expo <- attr(model, "exposure_col")
    if (!is.null(expo) && !expo %in% names(newdata)) {
      stop(sprintf("`newdata` must contain the exposure column `%s`.", expo),
           call. = FALSE)
    }
  }
  pr <- stats::predict(model, newdata = newdata, type = "link", se.fit = TRUE)
  z <- stats::qnorm(1 - (1 - level) / 2)
  fit <- as.numeric(pr$fit)
  se <- as.numeric(pr$se.fit)
  out <- data.frame(
    prediction = exp(fit),
    ci_lower = exp(fit - z * se),
    ci_upper = exp(fit + z * se),
    prob_at_least_one = 1 - exp(-exp(fit))
  )
  if (!is.null(newdata)) out <- cbind(newdata, out)
  rownames(out) <- NULL
  out
}

#' Check a Poisson model for overdispersion
#'
#' Computes the Pearson X2 / df ratio (close to 1 if Poisson fits), the
#' Pearson chi-squared test and the Cameron & Trivedi (1990) regression test
#' of Var = (1 + alpha) * mean against alpha > 0.
#'
#' @param model A `glm` with poisson or quasipoisson family.
#' @param alpha Significance level (default `0.05`).
#'
#' @return An object of class `overdispersion_check` (a list): `dispersion`,
#'   `chi2`, `df`, `p_pearson`, `alpha_ct`, `p_ct`, `conclusion`,
#'   `recommendation`.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(rain = rbinom(80, 1, 0.4))
#' d$y <- rpois(80, exp(1 + 0.3 * d$rain))
#' check_overdispersion(fit_poisson_model(d, y ~ rain))
check_overdispersion <- function(model, alpha = 0.05) {
  if (!inherits(model, "glm") ||
      !model$family$family %in% c("poisson", "quasipoisson")) {
    stop("`model` must be a glm with poisson or quasipoisson family.",
         call. = FALSE)
  }
  .check_level(alpha, "alpha")
  df <- model$df.residual
  if (df < 1L) stop("Not enough residual degrees of freedom.", call. = FALSE)

  chi2 <- sum(stats::residuals(model, type = "pearson")^2)
  phi <- chi2 / df
  p_over <- stats::pchisq(chi2, df, lower.tail = FALSE)
  p_under <- stats::pchisq(chi2, df)

  y <- model$y
  mu <- stats::fitted(model)
  aux <- ((y - mu)^2 - y) / mu
  aux <- aux[is.finite(aux)]
  alpha_ct <- NA_real_
  p_ct <- NA_real_
  if (length(aux) >= 3L) {
    fit_aux <- stats::lm(aux ~ 1)
    cf <- summary(fit_aux)$coefficients
    alpha_ct <- cf[1L, 1L]
    p_ct <- stats::pt(cf[1L, 3L], df = fit_aux$df.residual, lower.tail = FALSE)
  }

  if (p_over < alpha && phi > 1) {
    conclusion <- "Overdispersion detected"
    reco <- "Use family = 'quasipoisson' or a negative binomial model (MASS::glm.nb)."
  } else if (p_under < alpha && phi < 1) {
    conclusion <- "Underdispersion detected"
    reco <- "Check the data structure; a quasi-Poisson model may be suitable."
  } else {
    conclusion <- "No sign of over- or underdispersion"
    reco <- "The Poisson model is consistent with the observed dispersion."
  }
  structure(
    list(dispersion = phi, chi2 = chi2, df = df, p_pearson = p_over,
         alpha_ct = alpha_ct, p_ct = p_ct,
         conclusion = conclusion, recommendation = reco),
    class = "overdispersion_check"
  )
}

#' @export
print.overdispersion_check <- function(x, ...) {
  cat("Overdispersion check (Poisson model)\n")
  cat(sprintf("Pearson dispersion (X2/df): %.3f  [X2 = %.2f, df = %d]\n",
              x$dispersion, x$chi2, as.integer(x$df)))
  cat(sprintf("p-value (Pearson, overdispersion): %s\n",
              format.pval(x$p_pearson, digits = 4)))
  cat(sprintf("alpha (Cameron-Trivedi): %.3f | p-value: %s\n",
              x$alpha_ct, format.pval(x$p_ct, digits = 4)))
  cat("Conclusion:", x$conclusion, "\n")
  cat("Recommendation:", x$recommendation, "\n")
  invisible(x)
}
