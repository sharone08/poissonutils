#' Chi-Square Goodness-of-Fit Test for Poisson Distribution
#'
#' @param dataframe A data.frame containing your data.
#' @param variable_name A character string. The name of the column.
#' @return An htest object containing the test result.
#' @export
test_poisson <- function(dataframe, variable_name) {
  ajustement <- fit_poisson(dataframe, variable_name)
  donnees_colonne <- dataframe[[variable_name]]
  lambda_estime <- ajustement$lambda
  n <- length(donnees_colonne)

  obs <- as.vector(table(factor(donnees_colonne, levels = 0:max(donnees_colonne))))
  prob_theoriques <- dpois(0:max(donnees_colonne), lambda = lambda_estime)
  prob_theoriques[length(prob_theoriques)] <- 1 - sum(prob_theoriques[1:(length(prob_theoriques)-1)])
  attendu <- prob_theoriques * n

  indices_valides <- attendu >= 1
  if (sum(indices_valides) < 2) {
    stop("Insufficient data for a reliable test.")
  }

  obs_filtre <- obs[indices_valides]
  prob_filtre <- prob_theoriques[indices_valides]
  prob_filtre <- prob_filtre / sum(prob_filtre)

  test_res <- chisq.test(x = obs_filtre, p = prob_filtre)
  test_res$parameter <- c("df" = length(obs_filtre) - 2)
  test_res$p.value <- pchisq(test_res$statistic, df = test_res$parameter, lower.tail = FALSE)
  test_res$method <- "Chi-Square Goodness-of-Fit Test (Poisson)"

  return(test_res)
}
