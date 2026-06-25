resultats_kruskal <- function(data, code_indice_val, col_resultat) {
  
  if (inherits(data, "sf")) { 
    data <- sf::st_drop_geometry(data) 
  }
  
  modeles <- list()
  
  for (i in liste_indices) {
    
    data_filtre <- data %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(code_indice == i)
    
    form <- reformulate("cycle_DCE", response = col_resultat) 
    
    modele_kruskal <-  kruskal.test(form, data = data_filtre)
    
    # On stocke le résultat dans la liste, avec un nom explicite
    modeles[[as.character(i)]] <- modele_kruskal
  }
  
  return(modeles)
}



resultats_friedman <- function(data, code_indice_val, col_resultat, col_station) {
  
  if (inherits(data, "sf")) { 
    data <- sf::st_drop_geometry(data) 
  }
  
  modeles <- list()
  
  for (i in code_indice_val) {
    
    data_filtre <- data %>% 
      dplyr::filter(code_indice == i)
  
    compte <- data_filtre %>%
      dplyr::count(code_station, cycle_DCE) %>%
      dplyr::filter(n > 1)
    
    print(compte)
    
    cycle <- data_filtre %>%
      tidyr::pivot_wider(
        names_from = cycle_DCE,
        values_from = !!sym(col_resultat)
      ) %>%
      dplyr::filter(dplyr::if_any(everything(), is.na))
    
    print(cycle)
    
    # Construire la formule Friedman
    form <- as.formula(paste(col_resultat, "~ cycle_DCE |", col_station))
    
    modele_friedman <- friedman.test(form, data = data_filtre)
    
    modeles[[as.character(i)]] <- modele_friedman
  }
  
  return(modeles)
}