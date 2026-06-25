#' Effectuer une anova sur plusieurs indices et récupérer le modèle, le résumé
#' et les résidus
#'
#' @param data dataframe contenant les variables de travail
#' @param code_indice_val Variable du dataframe contenant les codes des indices ou obet les contenant
#' @param col_resultat variable du dataframe contenant les résultats des indices


resultats_anova <- function(data, code_indice_val, col_resultat) {
  
  if (inherits(data, "sf")) { 
    data <- sf::st_drop_geometry(data) 
    }
  
  modeles <- list()
  
  for (i in code_indice_val) {
    
    data_filtre <- data %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(code_indice == i)
    
    form <- reformulate("cycle_DCE", response = col_resultat) 
    
    modele_aov <- aov(form, data = data_filtre)
    
    modeles[[as.character(i)]] <- list(
      modele = modele_aov,
      summary = summary(modele_aov),
      residus = residuals(modele_aov),
      fitted = fitted.values(modele_aov)
    )
  }
  
  return(modeles)
}
