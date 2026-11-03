#' Calculate Poisson Distribution Probability
#'
#' @param x A positive integer or zero (the number of events).
#' @param lambda A number strictly greater than 0 (the mean).
#' @return The probability P(X = x) as a numeric value.
#' @export
#' @examples
#' calculate_probability(x = 2, lambda = 3)
calculate_probability <- function(x, lambda) {
  if (lambda <= 0) {
    stop("The lambda parameter must be strictly greater than 0.")
  }
  return(dpois(x, lambda))
}
