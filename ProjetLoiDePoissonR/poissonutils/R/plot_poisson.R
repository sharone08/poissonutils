#' Plot Poisson Distribution Fit on Real Data
#'
#' @param dataframe A data.frame containing your data.
#' @param variable_name A character string. The name of the column.
#' @return A ggplot2 graphic object.
#' @export
plot_poisson <- function(dataframe, variable_name) {
  ajustement <- fit_poisson(dataframe, variable_name)
  donnees_colonne <- dataframe[[variable_name]]
  lambda_estime <- ajustement$lambda

  table_reelle <- as.data.frame(table(donnees_colonne))
  names(table_reelle) <- c("Value", "Frequency")
  table_reelle$Value <- as.numeric(as.character(table_reelle$Value))
  table_reelle$Real_Proportion <- table_reelle$Frequency / sum(table_reelle$Frequency)

  valeurs_theoriques <- 0:max(donnees_colonne)
  df_theorique <- data.frame(
    Value = valeurs_theoriques,
    Theoretical_Proportion = dpois(valeurs_theoriques, lambda = lambda_estime)
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_bar(data = table_reelle, ggplot2::aes(x = Value, y = Real_Proportion, fill = "Real Data"), stat = "identity", alpha = 0.6) +
    ggplot2::geom_line(data = df_theorique, ggplot2::aes(x = Value, y = Theoretical_Proportion, color = "Poisson Law"), linewidth = 1) +
    ggplot2::geom_point(data = df_theorique, ggplot2::aes(x = Value, y = Theoretical_Proportion, color = "Poisson Law"), size = 3) +
    ggplot2::labs(title = paste("Fit for:", variable_name), subtitle = paste("Lambda =", round(lambda_estime, 3)), x = "x", y = "Proportion", fill = "Observations", color = "Theory") +
    ggplot2::theme_minimal()
  return(p)
}
