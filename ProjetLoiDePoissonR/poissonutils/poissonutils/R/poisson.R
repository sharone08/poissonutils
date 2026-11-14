#' Poisson probability
#'
#' The simple one: give a number of events `k` and the average rate `lambda`,
#' and get the probability of observing exactly `k` events,
#' \eqn{P(X = k) = e^{-\lambda} \lambda^k / k!}.
#'
#' @param k Number of events (whole number `>= 0`). Can be a vector.
#' @param lambda Average number of events per unit (numeric `>= 0`). Use
#'   [estimate_lambda()] to get it from data.
#' @param type `"exact"` (default) for \eqn{P(X = k)}, `"cumulative"` for
#'   \eqn{P(X \le k)}, or `"upper"` for \eqn{P(X > k)}.
#'
#' @return A numeric vector of probabilities.
#' @export
#' @examples
#' poisson_prob(2, lambda = 3)                       # P(X = 2)
#' poisson_prob(2, lambda = 3, type = "cumulative")  # P(X <= 2)
#' poisson_prob(5, lambda = 3, type = "upper")       # P(X > 5)
#' poisson_prob(0:4, lambda = 3)                     # several values at once
poisson_prob <- function(k, lambda, type = c("exact", "cumulative", "upper")) {
  type <- match.arg(type)
  .check_lambda(lambda)
  if (!is.numeric(k) || length(k) == 0L || anyNA(k) || any(k < 0) ||
      any(abs(k - round(k)) > 1e-8)) {
    stop("`k` must contain whole numbers >= 0.", call. = FALSE)
  }
  switch(type,
    exact = stats::dpois(k, lambda),
    cumulative = stats::ppois(k, lambda),
    upper = stats::ppois(k, lambda, lower.tail = FALSE)
  )
}

#' Table of Poisson probabilities
#'
#' @param lambda Average number of events (a single number `>= 0`).
#' @param k_max Largest value of `k` in the table (whole number `>= 0`).
#' @param days Number of days in the period (e.g. `365`). If given, adds
#'   `expected_days`: days expected with exactly `k` events.
#'
#' @return A data.frame with `k`, `prob` (P(X = k)), `cum_prob` (P(X <= k)),
#'   `upper_prob` (P(X > k)) and optionally `expected_days`.
#' @export
#' @examples
#' poisson_table(lambda = 3, k_max = 10)
#' poisson_table(lambda = 3, k_max = 10, days = 365)
poisson_table <- function(lambda, k_max, days = NULL) {
  .check_lambda(lambda)
  if (length(lambda) != 1L) stop("`lambda` must be a single number.", call. = FALSE)
  if (!is.numeric(k_max) || length(k_max) != 1L || is.na(k_max) || k_max < 0 ||
      abs(k_max - round(k_max)) > 1e-8) {
    stop("`k_max` must be a whole number >= 0.", call. = FALSE)
  }
  k <- 0:as.integer(round(k_max))
  res <- data.frame(
    k = k,
    prob = stats::dpois(k, lambda),
    cum_prob = stats::ppois(k, lambda),
    upper_prob = stats::ppois(k, lambda, lower.tail = FALSE)
  )
  if (!is.null(days)) {
    if (!is.numeric(days) || length(days) != 1L || is.na(days) || days <= 0) {
      stop("`days` must be a strictly positive number.", call. = FALSE)
    }
    res$expected_days <- res$prob * days
  }
  res
}

#' Estimate lambda with an exact confidence interval
#'
#' Estimates the Poisson rate by the sample mean, with an exact (Garwood)
#' confidence interval and the dispersion index variance / mean (close to 1
#' for Poisson data).
#'
#' @param x Numeric vector of counts (whole numbers `>= 0`). `NA`s are dropped.
#' @param level Confidence level (default `0.95`).
#'
#' @return A one-row data.frame: `n`, `total`, `lambda`, `variance`,
#'   `dispersion_index`, `ci_lower`, `ci_upper`.
#' @export
#' @examples
#' estimate_lambda(c(2, 3, 4, 3, 5, 1, 2))
estimate_lambda <- function(x, level = 0.95) {
  .check_counts(x, "x")
  .check_level(level)
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2L) stop("At least 2 observations are required.", call. = FALSE)
  s <- sum(x)
  a <- 1 - level
  lower <- if (s == 0) 0 else stats::qchisq(a / 2, 2 * s) / (2 * n)
  upper <- stats::qchisq(1 - a / 2, 2 * (s + 1)) / (2 * n)
  m <- mean(x)
  v <- stats::var(x)
  data.frame(
    n = n, total = s, lambda = m, variance = v,
    dispersion_index = if (m > 0) v / m else NA_real_,
    ci_lower = lower, ci_upper = upper
  )
}

#' Chi-squared goodness-of-fit test for the Poisson distribution
#'
#' Compares the observed distribution of counts with a Poisson distribution
#' whose rate is estimated from the data. Classes with expected count below
#' `min_expected` are pooled (tail first, then head). Degrees of freedom are
#' `classes - 2` (one estimated parameter).
#'
#' @param x Numeric vector of counts (at least 10 observations).
#' @param min_expected Minimum expected count per class (default `5`).
#' @param alpha Significance level (default `0.05`).
#'
#' @return An object of class `poisson_gof` (a list) with `table`, `lambda`,
#'   `n`, `statistic`, `df`, `p_value` and `conclusion`.
#' @export
#' @examples
#' x <- rep(0:8, times = round(1000 * dpois(0:8, 3)))
#' poisson_gof_test(x)
poisson_gof_test <- function(x, min_expected = 5, alpha = 0.05) {
  .check_counts(x, "x")
  .check_level(alpha, "alpha")
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 10L) stop("At least 10 observations are required.", call. = FALSE)
  lambda <- mean(x)
  if (lambda <= 0) {
    stop("All observations are zero: the test is not possible.", call. = FALSE)
  }
  kmax <- as.integer(round(max(x)))
  k <- 0:kmax
  obs <- as.numeric(tabulate(as.integer(round(x)) + 1L, nbins = kmax + 1L))
  p <- stats::dpois(k, lambda)
  p[kmax + 1L] <- stats::ppois(kmax - 1L, lambda, lower.tail = FALSE)
  expected <- n * p
  lo <- k
  hi <- k
  hi[kmax + 1L] <- Inf

  while (length(expected) > 2L && expected[length(expected)] < min_expected) {
    m <- length(expected)
    obs[m - 1L] <- obs[m - 1L] + obs[m]
    expected[m - 1L] <- expected[m - 1L] + expected[m]
    hi[m - 1L] <- hi[m]
    obs <- obs[-m]; expected <- expected[-m]; lo <- lo[-m]; hi <- hi[-m]
  }
  while (length(expected) > 2L && expected[1L] < min_expected) {
    obs[2L] <- obs[1L] + obs[2L]
    expected[2L] <- expected[1L] + expected[2L]
    lo[2L] <- lo[1L]
    obs <- obs[-1L]; expected <- expected[-1L]; lo <- lo[-1L]; hi <- hi[-1L]
  }

  contribution <- (obs - expected)^2 / expected
  stat <- sum(contribution)
  df <- length(expected) - 2L
  if (df < 1L) {
    warning("Too few classes after pooling: p-value not computed.", call. = FALSE)
    pval <- NA_real_
  } else {
    pval <- stats::pchisq(stat, df, lower.tail = FALSE)
  }
  class_label <- ifelse(is.infinite(hi), paste0(">=", lo),
                        ifelse(lo == hi, as.character(lo), paste0(lo, "-", hi)))
  conclusion <- if (is.na(pval)) {
    "Inconclusive test"
  } else if (pval < alpha) {
    "Reject H0: the distribution departs from a Poisson law"
  } else {
    "Do not reject H0: distribution compatible with a Poisson law"
  }
  structure(
    list(
      table = data.frame(class = class_label, observed = obs,
                         expected = expected, contribution = contribution),
      lambda = lambda, n = n, statistic = stat, df = df,
      p_value = pval, conclusion = conclusion
    ),
    class = "poisson_gof"
  )
}

#' @export
print.poisson_gof <- function(x, ...) {
  cat("Chi-squared goodness-of-fit test for the Poisson distribution\n")
  cat(sprintf("n = %d | estimated lambda = %.4f\n", x$n, x$lambda))
  cat(sprintf("X2 = %.3f | df = %d | p-value = %s\n",
              x$statistic, x$df, format.pval(x$p_value, digits = 4)))
  cat("Conclusion:", x$conclusion, "\n\n")
  print(x$table, row.names = FALSE)
  invisible(x)
}
