create_table <- function(data,
                         id_station){
  
  data_2 <- data %>% 
    filter(sta_code_sandre==id_station) %>% 
    mutate(proj="Lambert93") %>% 
    select(,
           libelle_region,
           libelle_departement,
           libelle_commune,
           bas_libelle_sandre,
           libelle_cours_eau,
           sta_code_sandre,
           sta_libelle_sandre,
           obj_libelle,
           proj,
           sta_coordonnees_x,
           sta_coordonnees_y,
           libelle_typologie) %>% 
    rename(`Département`=libelle_departement,
           `Région`=libelle_region,
           `Commune`=libelle_commune,
           `Bassin hydrographique`=bas_libelle_sandre,
           `Cours d'eau`=libelle_cours_eau,
           `Code Sandre station`=sta_code_sandre,
           `Nom de la station`=sta_libelle_sandre,
           `Réseau`=obj_libelle,
           `Système projection coordonnées`=proj,
           `Coordonnées X de la station`=sta_coordonnees_x,
           `Coordonnées Y de la station`=sta_coordonnees_y,
           Typologie = libelle_typologie
    )
  
  data_filtre <- data_2 %>%
    mutate(across(everything(), as.character)) %>% 
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "valeur"
    )
  
  info_sta <- data_filtre %>%
    gt() %>% 
    tab_options(
      column_labels.hidden = TRUE,     # cacher les noms de colonnes
      table.font.size = 13,            # taille de police
      table.border.top.color = "black",
      table.border.bottom.color = "black",
      table.border.top.width = px(2),
      table.border.bottom.width = px(2)
    ) %>% 
    cols_align(align = "left") %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = 1)   # première colonne en gras
    )
  return(info_sta)
}