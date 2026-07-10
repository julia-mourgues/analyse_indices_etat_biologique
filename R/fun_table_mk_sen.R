#' Générer les tables présentant les résultats des test Mann-Kendall et Sen-Theil de façon globale, à l'échelle Normandie
#'
#' @param data dataframe contenant les résultats
#' @param periode_etudiee argument utilisé pour compléter le titre.

table_pentes_globales <- function (data, periode_etudiee) {
  data_filtre <- data %>%
    dplyr::select(-nb_annees, -annee_max, -annee_min) %>%
    dplyr::mutate(
      #inverser les tendances de l'IPR
      sens_slope_corrige = dplyr::if_else(libelle_indice == "IPR", 
                                          -sens_slope, 
                                          sens_slope),
      #créer une variable avec la direction de la tendance
      tendance = dplyr::case_when(
        mk_pvalue < 0.05 & sens_slope_corrige > 0 ~ "Tendance positive",
        mk_pvalue < 0.05 &
          sens_slope_corrige < 0 ~ "Tendance négative",
        TRUE ~ "Pas de tendance"
      ),
      mk_pvalue = base::round(mk_pvalue, 4),
      sens_slope_corrige = base::signif(sens_slope_corrige, 3)
    ) %>%
    dplyr::select(-sens_slope)
  
  table <- data_filtre %>%
    gt::gt() %>%
    gt::tab_header(
      title = "Résultats des tests Mann-Kendall et pente de Sen",
      subtitle = paste0(
        "Analyse des indices d'état écologiques (",
        periode_etudiee,
        ")"
      )
    ) %>%
    gt::cols_label(
      libelle_indice = "Indice",
      mk_pvalue = "p-value MK",
      sens_slope_corrige = "Pente de Sen",
      tendance = "Interprétation"
    )  %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(
        columns = c(sens_slope_corrige),
        rows = mk_pvalue < 0.05
      )
    )
  
  
  gt::gtsave(table, filename = "../Output/1_Tendance_indice/02_Pentes_mk_sen/Tables/table_pentes_tendances_globales.pdf")
}



#' Générer les tables présentant les résultats des test Mann-Kendall et Sen-Theil pour chaque station
#'
#' @param data dataframe contenant les résultats
#' @param periode_etudiee argument utilisé pour compléter le titre.

table_pentes_stations <- function (data, periode_etudiee) {
  #Préparation des données
  data_filtre <- data %>%
    dplyr::mutate(
      sens_slope_corrige = dplyr::if_else(libelle_indice == "IPR", -sens_slope, sens_slope),
      signif = dplyr::case_when(
        mk_pvalue < 0.001 ~ "***",
        mk_pvalue < 0.01  ~ "**",
        mk_pvalue < 0.05  ~ "*",
        TRUE              ~ ""
      ),
      pente_sig = paste0(base::round(sens_slope_corrige, 3), " ", signif)
    ) %>%
    dplyr::select(-sens_slope)
  
  data_large <- data_filtre %>%
    dplyr::select(code_departement,
                  libelle_station,
                  libelle_indice,
                  pente_sig,
                  signif) %>%
    tidyr::pivot_wider(names_from = libelle_indice,
                       values_from = c(pente_sig, signif))
  
  indices <- c("IPR", "IBMR", "IBD", "I2M2")
  
  data_clean <- data_large %>%
    dplyr::arrange(code_departement, libelle_station) %>%
    dplyr::mutate(code_departement = as.character(code_departement))
  
  # Supprimer les préfixes/suffixes dans tous les noms de colonnes
  names(data_clean) <- names(data_clean) %>%
    base::gsub("pente_sig_", "", x = .) %>%     # retire le prefixe
    base::gsub("signif_", "sig_", x = .)        # harmonise "signif_" → "sig_"
  
  # Colonnes pentes
  cols_pente_clean <- base::intersect(names(data_clean), indices)
  
  # Colonnes de significativité correspondantes
  cols_signif_clean <- base::grep("^sig_", names(data_clean), value = TRUE)
  
  # Construction du tableau final
  table <- data_clean %>%
    dplyr::select(
      code_departement,
      libelle_station,
      dplyr::all_of(cols_pente_clean),
      dplyr::all_of(cols_signif_clean)
    ) %>%
    gt::gt() %>%
    gt::cols_label(code_departement = "Département", libelle_station  = "Station") %>%
    gt::tab_header(
      title = gt::md("**Tendances des indices pour chaque station.**"),
      subtitle = paste0(
        "Pente de Sen + significativité (*, **, ***). Période étudiée: ",
        periode_etudiee,
        "."
      )
    ) %>%
    gt::cols_hide(cols_signif_clean)    # on masque les colonnes sig_
  
  # Application du gras quand significatif
  for (indice in cols_pente_clean) {
    col_sig <- paste0("sig_", indice)
    
    table <- table %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(
          columns = indice,
          rows = !!as.name(col_sig) != ""
        )
      )
  }
  gt::gtsave(table, filename = "../Output/1_Tendance_indice/02_Pentes_mk_sen/Tables/table_pentes_stations.pdf")
}



#' Générer les tables présentant les tendances (avec symboles) pour chaque station
#'
#' @param data dataframe contenant les résultats
#' @param periode_etudiee argument utilisé pour compléter le titre.

table_tendances_stations <- function (data, periode_etudiee) {
  #Préparation des données
  data_filtre <- data %>%
    dplyr::mutate(
      sens_slope_corrige = dplyr::if_else(libelle_indice == "IPR", -sens_slope, sens_slope),
      signif = dplyr::case_when(
        mk_pvalue < 0.001 ~ "***",
        mk_pvalue < 0.01  ~ "**",
        mk_pvalue < 0.05  ~ "*",
        TRUE              ~ ""
      ),
      pente_sig = paste0(base::round(sens_slope_corrige, 3), " ", signif)
    ) %>%
    dplyr::select(-sens_slope)
  
  data_tendance <- data_filtre %>%
    dplyr::select(-nb_annees, -annee_min, -annee_max) %>%
    dplyr::mutate(
      type_tendance = dplyr::if_else(sens_slope_corrige > 0, "Amélioration", "Déterioration"),
      fleche_sig = dplyr::case_when(
        signif == "" &
          type_tendance == "Amélioration" ~ "<span style='color:rgba(128,128,128,0.35);font-size:18px'>&#x25B2;</span>",
        signif == "" &
          type_tendance == "Déterioration" ~ "<span style='color:rgba(128,128,128,0.35);font-size:18px'>&#x25BC;</span>",
        signif != "" &
          type_tendance == "Amélioration" ~ "<span style='color:green;font-size:18px'>&#x25B2;</span>",
        signif != "" &
          type_tendance == "Déterioration" ~ "<span style='color:red;font-size:18px'>&#x25BC;</span>"
      )
    )
  
  
  #Pivoter au format large
  data_tendance_large <- data_tendance %>%
    dplyr::select(code_departement,
                  libelle_station,
                  libelle_indice,
                  fleche_sig) %>%
    tidyr::pivot_wider(names_from = libelle_indice, values_from = fleche_sig)
  
  tendance_clean <- data_tendance_large %>%
    dplyr::arrange(code_departement, libelle_station) %>%
    dplyr::mutate(code_departement = base::as.character(code_departement))
  
  indices <- c("IPR", "IBMR", "IBD", "I2M2")
  
  # Colonnes réellement présentes correspondant aux indices
  cols_fleche_clean <- base::intersect(names(tendance_clean), indices)
  
  # Construction du tableau global
  table_tendance_all <- tendance_clean %>%
    dplyr::select(code_departement,
                  libelle_station,
                  dplyr::all_of(cols_fleche_clean)) %>%
    gt::gt() %>%
    # Renommer les en-têtes demandés
    gt::cols_label(code_departement = "Département", libelle_station  = "Station") %>%
    gt::tab_header(
      title = gt::md("**Tendances des indices pour chaque station.**"),
      subtitle = gt::md(
        paste0(
          "Flèche de tendance (triangle vert = **Amélioration** ; triangle rouge = **Dégradation**; triangle gris = Tendance non significative).
          Période étudiée: ",
          periode_etudiee,
          "."
        )
      )
    ) %>%
    # Les colonnes d'indices contiennent du Markdown (triangles, etc.)
    gt::fmt_markdown(columns = dplyr::all_of(cols_fleche_clean))
  
  # Export
  gt::gtsave(table_tendance_all, filename = "../Output/1_Tendance_indice/02_Pentes_mk_sen/Tables/table_tendances_stations.pdf")
}









table_tendances_metriques_stations <- function(data,
                                               periode_etudiee,
                                               metriques,
                                               label_group,
                                               nom_fichier) {
  # --- Préparation des données ---
  data_filtre <- data %>%
    dplyr::mutate(
      signif = dplyr::case_when(
        mk_pvalue < 0.001 ~ "***",
        mk_pvalue < 0.01  ~ "**",
        mk_pvalue < 0.05  ~ "*",
        TRUE              ~ ""
      ),
      pente_sig = paste0(round(sens_slope, 3), " ", signif)
    )
  
  data_tendance <- data_filtre %>%
    dplyr::select(-nb_annees, -annee_min, -annee_max) %>%
    dplyr::mutate(
      type_tendance = if_else(sens_slope > 0, "Amélioration", "Déterioration"),
      fleche_sig = case_when(
        signif == "" &
          type_tendance == "Amélioration" ~ "<span style='color:rgba(128,128,128,0.35);font-size:18px'>&#x25B2;</span>",
        signif == "" &
          type_tendance == "Déterioration" ~ "<span style='color:rgba(128,128,128,0.35);font-size:18px'>&#x25BC;</span>",
        signif != "" &
          type_tendance == "Amélioration" ~ "<span style='color:green;font-size:18px'>&#x25B2;</span>",
        signif != "" &
          type_tendance == "Déterioration" ~ "<span style='color:red;font-size:18px'>&#x25BC;</span>"
      )
    )
  
  data_tendance_large <- data_tendance %>%
    select(
      code_departement,
      libelle_station,
      reseaux,
      libelle_support,
      libelle_metrique,
      fleche_sig
    ) %>%
    tidyr::pivot_wider(names_from = libelle_support, values_from = fleche_sig)
  
  tendance_clean <- data_tendance_large %>%
    arrange(code_departement, libelle_station) %>%
    mutate(code_departement = as.character(code_departement))
  
  cols_fleche_clean <- intersect(names(tendance_clean), metriques)
  
  # --- Construction du tableau ---
  table_tendance_all <- tendance_clean %>%
    select(code_departement,
           libelle_station,
           reseaux,
           all_of(cols_fleche_clean)) %>%
    gt::gt() %>%
    tab_spanner(label = label_group, columns = c(all_of(metriques))) %>%
    gt::tab_options(table.font.size = px(11)) %>%
    gt::cols_label(
      code_departement = "Département",
      libelle_station  = "Station",
      reseaux          = "Réseau"
    ) %>%
    gt::tab_header(
      title = gt::md("**Tendances des métriques pour chaque station.**"),
      subtitle = gt::md(
        paste0(
          "Flèche de tendance (triangle vert = **Amélioration** ; triangle rouge = **Dégradation** ; triangle gris = Tendance non significative).
          Période étudiée : ",
          periode_etudiee,
          "."
        )
      )
    ) %>%
    gt::fmt_markdown(columns = all_of(cols_fleche_clean))
  
  # --- Export PDF ---
  gt::gtsave(table_tendance_all, filename = nom_fichier)
}
