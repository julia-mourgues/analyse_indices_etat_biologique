# Attribuer les classes d'état écologiques et les couleurs correspondantes aux résultats des EQR

valeurs_seuils <- readRDS(here::here("Data/clean_data", "valeurs_seuils_indices.rds"))


attribuer_classe_eco <- function(data, table_seuils=valeurs_seuils, var_eqr, var_temp) {
  
  data_classe <- data%>%
    dplyr::left_join(
      table_seuils,
      by = c(
        "code_station"   = "CODE_STATION",
        "libelle_indice" = "indice",
        "Typologie"      = "TYPO_NATIONALE"
      )
    ) %>%
    dplyr::filter(
      .data[[var_eqr]] >= seuil_bas &
      .data[[var_eqr]] <= seuil_haut
    ) %>%
    dplyr::distinct(
      code_station, libelle_indice, Typologie, .data[[var_temp]],
      .keep_all = TRUE
    )
  return(data_classe)
}



#' Créer les classes d'état écologique qui vont permettre de construire les rectangles de fond pour les graphiques
#'
#' @param col_code Variable du dataframe contenant les codes des indices
#' @param bornes_indices listes contenant les bornes inférieures et supérieures des classes de chaque indice

creation_classes <- function(col_code, bornes_indices) {
  
  b <- bornes_indices[[col_code]]
  
  # Libellés homogènes 
  classe_libelle <- c("Mauvais", "Médiocre", "Moyen", "Bon", "Très bon") 
  classe_couleur <- b$couleurs
  
  # Inversion de la légende UNIQUEMENT pour 7036
  if (col_code %in% c("7036", "IPR")) {
    classe_libelle <- rev(classe_libelle)
    classe_couleur <- rev(classe_couleur)
  }
  
  data.frame(
    libelle_indice = col_code, 
    classe_borne_inf = b$borne_inf,
    classe_borne_sup = b$borne_sup,
    classe_libelle = factor(
      classe_libelle,
      levels = c("Mauvais", "Médiocre", "Moyen", "Bon", "Très bon")
    ),
    classe_couleur = classe_couleur,
    stringsAsFactors = FALSE
  )
}


