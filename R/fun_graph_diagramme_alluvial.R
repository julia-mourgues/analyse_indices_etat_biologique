#' Créer les graphiques présentant l'évolution des proportions des classes écologiques pour chaque indice au cours du temps.
#'
#' @param data dataframe contenant les résultats
#' @param liste_reseaux liste des réseaux étudiés
#' @param reseau_titre  Nom du ou des réseaux étudiés, utilisés pour compléter le titre et le nom des fichiers


diag_alluvial <- function (data,
                           liste_reseaux,
                           output_dir = "../Output/4_Proportions_classes_eco/03_Sankey") {
  
  
  
  #Construire automatiquement le "titre réseau"
  reseau_titre <- paste(sort(unique(liste_reseaux)), collapse = "_")
  
  # S'assurer que le répertoire de sortie existe
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  
  data_filtre <- data %>% 
    dplyr::filter(.data$reseaux %in% liste_reseaux) %>% 
    dplyr::arrange(code_station, annee)
  
  if (length(liste_reseaux) == 1 && liste_reseaux == "RRP" ) {
    data_filtre <- data_filtre %>%
      dplyr::filter(annee>=2013)
    
  }
  
  #Extraire les indices
  all_indices <- unique(data_filtre$libelle_indice) #extraire les indices
  
  for (idx in all_indices) {
    dl <- dplyr::filter(data_filtre, .data$libelle_indice == !!idx)

    # Filtrer les données pour l'indice courant
    data_filtre_idx <- data_filtre |> 
      dplyr::filter(libelle_indice == idx)
    
    #Passage au format large
    data_large <- data_filtre_idx %>%
      tidyr::pivot_wider(
        id_cols = code_station,
        names_from = annee, 
        values_from = libelle_classe)
    
    
    #Extraire les colonnes correspondant aux années
    year_cols <- setdiff(names(data_large), c("code_station"))
    
    # Ordre chronologique des axes
    years_num <- sort(suppressWarnings(as.numeric(year_cols)))
    years_kept <- as.character(years_num)
    
    
    #Passage au format lodes (ggalluvial)
    data_lodes <- ggalluvial::to_lodes_form(
      data_large,
      key = "annee",
      axes = years_kept,
      value = "etat"
    )
    
    data_lodes$etat <- forcats::fct_explicit_na(
      data_lodes$etat,
      na_level = "Non évalué"
    )
    
    data_lodes$annee <- base::factor(data_lodes$annee, 
                                     levels = years_kept)
    
    data_lodes$etat <- factor(
      data_lodes$etat,
      levels = c("Très bon", "Bon", "Moyen", "Médiocre","Mauvais" , "Non évalué"),
      ordered = TRUE
    )
    
    if (length(liste_reseaux) == 1 && liste_reseaux == "RCS" && idx %in% c("IPR","IBMR")) {
      data_lodes$annee_num <- as.integer(as.character(data_lodes$annee))
      data_lodes <- data_lodes %>%
        dplyr::mutate(pair_impair = dplyr::case_when(
          as.numeric(annee_num) %% 2 == 0 ~ "Année paire",
          TRUE                        ~ "Année impaire"
        ))
      
    }
    

    #Définir les couleurs associées aux niveaux
    couleurs  = c("white" ,"red","orange","yellow","lightgreen","#2b83ba")
    classes   = c("Non évalué","Mauvais", "Médiocre", "Moyen", "Bon", "Très bon")
    palette_etat <- stats::setNames(couleurs, classes)
    
    max_stations <- length(unique(data_lodes$code_station))
    limite_y <- ceiling(max_stations / 10) * 10
 
    
    plot_sankey_idx <- ggplot2::ggplot(
      data_lodes,
      ggplot2::aes(
        x        = annee,
        stratum  = etat,
        alluvium = code_station,
        y        = 1,             # chaque station contribue de façon égale
        fill     = etat
      )
    ) +
      ggalluvial::geom_flow(ggplot2::aes(alpha = etat),color="darkgrey") +
      ggalluvial::geom_stratum() +
      ggplot2::scale_fill_manual(values = palette_etat, drop = TRUE) +
      ggplot2::scale_alpha_manual(
        values = c(
          "Très bon"    = 0.6,
          "Bon"         = 0.6,
          "Moyen"       = 0.6,
          "Médiocre"    = 0.6,
          "Mauvais"     = 0.6,
          "Non évalué"  = 0.15   # très léger
        ),
        guide = "none"
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, limite_y),
        breaks = seq(0, limite_y, by = 10)
      ) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title    = paste0("Diagramme alluvial – évolution du nombre de stations par classe d’état pour l'indice ", idx, "."),
        subtitle = paste0("Réseaux : ", reseau_titre,"."),
        x        = "Années",
        y        = "Nombre de stations",
        fill     = "État écologique"
      ) +
      ggplot2::theme(
        plot.title   = ggtext::element_textbox_simple(size = 14, face = "bold", margin = ggplot2::margin(b = 5)),
        plot.subtitle= ggtext::element_textbox_simple(size = 13, margin = ggplot2::margin(b = 10)),
        axis.title   = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 11),
        legend.title = ggplot2::element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title.position = "top"
      )+
      ggplot2::guides(
        fill  = guide_legend(order = 1, ncol = 6))
    
    if (length(liste_reseaux) == 1 && liste_reseaux == "RCS" && idx%in% c("IPR","IBMR")) {
      plot_sankey_idx <- plot_sankey_idx +
        ggplot2::facet_wrap(~pair_impair, scales="free_x")
    }
    
    ggplot2::ggsave(
      filename = file.path(output_dir, paste0("sankey_", reseau_titre, "_", idx, ".png")),
      plot     = plot_sankey_idx,
      width    = 14,
      height   = 8
    )
  }
  
}


diag_alluvial_DCE <- function (data,
                           liste_reseaux,
                           output_dir = "../Output/4_Proportions_classes_eco/03_Sankey/Cycle_DCE") {
  
  
  
  #Construire automatiquement le "titre réseau"
  reseau_titre <- paste(sort(unique(liste_reseaux)), collapse = "_")
  
  # S'assurer que le répertoire de sortie existe
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  
  data_filtre <- data %>% 
    dplyr::filter(.data$reseaux %in% liste_reseaux) %>% 
    dplyr::arrange(code_station, cycle_DCE)
  
  if (length(liste_reseaux) == 1 && liste_reseaux == "RRP" ) {
    data_filtre <- data_filtre %>%
      dplyr::filter(cycle_DCE !="2004-2012")
    
  }
  
  #Extraire les indices
  all_indices <- unique(data_filtre$libelle_indice) #extraire les indices
  
  for (idx in all_indices) {
    dl <- dplyr::filter(data_filtre, .data$libelle_indice == !!idx)
    
    # Filtrer les données pour l'indice courant
    data_filtre_idx <- data_filtre |> 
      dplyr::filter(libelle_indice == idx)
    
    #Passage au format large
    data_large <- data_filtre_idx %>%
      tidyr::pivot_wider(
        id_cols = code_station,
        names_from = cycle_DCE, 
        values_from = libelle_classe)

    #Extraire les colonnes correspondant aux années
    cycle_cols <- setdiff(names(data_large), c("code_station"))
    
    
    #Passage au format lodes (ggalluvial)
    data_lodes <- ggalluvial::to_lodes_form(
      data_large,
      key = "cycle",
      axes = cycle_cols,
      value = "etat"
    )
    
    data_lodes$etat <- forcats::fct_explicit_na(
      data_lodes$etat,
      na_level = "Non évalué"
    )
    
    data_lodes$cycle <- base::factor(data_lodes$cycle, 
                                     levels = c("2004-2012","2013-2018","2019-2024"))
    
    data_lodes$etat <- factor(
      data_lodes$etat,
      levels = c("Très bon", "Bon", "Moyen", "Médiocre","Mauvais" , "Non évalué"),
      ordered = TRUE
    )
    
    #Définir les couleurs associées aux niveaux
    couleurs  = c("white" ,"red","orange","yellow","lightgreen","#2b83ba")
    classes   = c("Non évalué","Mauvais", "Médiocre", "Moyen", "Bon", "Très bon")
    palette_etat <- stats::setNames(couleurs, classes)
    
    max_stations <- length(unique(data_lodes$code_station))
    limite_y <- ceiling(max_stations / 10) * 10
    
    
    plot_sankey_idx <- ggplot2::ggplot(
      data_lodes,
      ggplot2::aes(
        x        = cycle,
        stratum  = etat,
        alluvium = code_station,
        y        = 1,             # chaque station contribue de façon égale
        fill     = etat
      )
    ) +
      ggalluvial::geom_flow(ggplot2::aes(alpha = etat), color="darkgrey") +
      ggalluvial::geom_stratum() +
      ggplot2::scale_fill_manual(values = palette_etat, drop = TRUE) +
      ggplot2::scale_alpha_manual(
        values = c(
          "Très bon"    = 0.6,
          "Bon"         = 0.6,
          "Moyen"       = 0.6,
          "Médiocre"    = 0.6,
          "Mauvais"     = 0.6,
          "Non évalué"  = 0.15   # très léger
        ),
        guide = "none"
      ) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title    = paste0("Diagramme alluvial – évolution du nombre de stations par classe d’état entre les différents cycles DCE, pour l'indice ", idx, "."),
        subtitle = paste0("Réseaux : ", reseau_titre),
        x        = "Cycle DCE",
        y        = "Nombre de stations",
        fill     = "État écologique"
      ) +
      ggplot2::theme(
        plot.title   = ggtext::element_textbox_simple(size = 14, face = "bold", margin = ggplot2::margin(b = 10)),
        plot.subtitle= ggtext::element_textbox_simple(size = 13, margin = ggplot2::margin(b = 5)),
        axis.title   = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 11),
        legend.title = ggplot2::element_text(size = 11, face = "bold")
      )
    
    ggplot2::ggsave(
      filename = file.path(output_dir, paste0("sankey_DCE_", reseau_titre, "_", idx, ".png")),
      plot     = plot_sankey_idx,
      width    = 14,
      height   = 10
    )
  }
  
}