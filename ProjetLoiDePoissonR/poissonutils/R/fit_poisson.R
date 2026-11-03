#' Fit a Poisson Distribution to Any Dataset
#'
#' @param dataframe A data.frame (your dataset).
#' @param variable_name A character string. The exact name of the column.
#' @return A list containing the variable name and the estimated lambda value.
#' @export
#' @examples
#' my_table <- data.frame(clicks = c(12, 15, 8, 14, 11))
#' fit_poisson(dataframe = my_table, variable_name = "clicks")
fit_poisson <- function(dataframe, variable_name) {
  if (!is.data.frame(dataframe)) {
    stop("The 'dataframe' argument must be a data frame (data.frame).")
  }
  if (!variable_name %in% colnames(dataframe)) {
    stop(paste("The column", variable_name, "cannot be found in the dataset."))
  }

  donnees_colonne <- dataframe[[variable_name]]

  if (any(donnees_colonne < 0) || any(donnees_colonne %% 1 != 0)) {
    stop("The selected column must contain only positive integers or zero (counts).")
  }

  lambda_estime <- mean(donnees_colonne)
  return(list(variable = variable_name, lambda = lambda_estime))
}
