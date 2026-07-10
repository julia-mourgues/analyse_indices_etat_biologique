#' Créer les graphiques présentant l'évolution des proportions des classes écologiques pour chaque indice au cours du temps.
#'
#' @param data dataframe contenant les résultats
#' @param liste_reseaux liste des réseaux étudiés
#' @param nb_min_station  nombre minimum de station nécessaire par année

barplot_proportion <- function(data,
                               liste_reseaux,
                               nb_min_station,
                               output_dir = "../Output/4_Proportions_classes_eco/01_Barplot") {

  #Construire automatiquement le "titre réseau"
  reseau_titre <- paste(sort(unique(liste_reseaux)), collapse = "_")
  
  # S'assurer que le répertoire de sortie existe
  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)
  
  #filtrer et réarranger les données
  data_filtre <- data %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$reseaux %in% liste_reseaux)
  
  nb_annee <- data_filtre %>%
    dplyr::group_by(libelle_indice, annee) %>%
    dplyr::summarise(nb_station = dplyr::n_distinct(libelle_station)) %>%
    dplyr::filter(nb_station >= nb_min_station)
  
  data_filtre2 <- data_filtre %>%
    dplyr::inner_join(nb_annee, by = c("libelle_indice", "annee"))
  
  #calcul des proportions
  data_pct <- data_filtre2 %>%
    dplyr::group_by(code_indice, libelle_indice, annee, libelle_classe) %>%
    dplyr::summarise(n_classe = dplyr::n_distinct(code_station),
                     .groups = "drop") %>%
    dplyr::group_by(code_indice, libelle_indice, annee) %>%
    dplyr::mutate(n_total = sum(n_classe),
                  pct = (n_classe / n_total) * 100) %>%
    dplyr::ungroup()
  
  data_pct$libelle_classe <- factor(
    data_pct$libelle_classe,
    levels = c("Très bon", "Bon", "Moyen", "Médiocre", "Mauvais"),
    ordered = TRUE
  )
  
  #définir les classes d'état et leur couleur
  couleurs  = c("red", "orange", "yellow", "lightgreen", "#2b83ba")
  classes   = c("Mauvais", "Médiocre", "Moyen", "Bon", "Très bon")
  palette_etat <- setNames(couleurs, classes)
  
  
  #graphique présentant le pourcentage de station dans chaque état biologique au cours du temps
  g1 <- ggplot2::ggplot(data_pct, ggplot2::aes(x = annee, 
                                               y = pct, 
                                               fill = libelle_classe)) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::facet_wrap(~ libelle_indice, ncol = 4) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(
      values = palette_etat,
      breaks = c("Très bon", "Bon", "Moyen", "Médiocre", "Mauvais")
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = paste0(
        "Proportion des classes d'état écologique des stations appartenant au réseaux ",
        reseau_titre,
        " par année."
      ),
      subtitle = paste0(
        "Les histogrammes tracés en dessous présentent le nombre de stations prises en comptes pour le calcul des proportions.
         Les années avec moins de ",
        nb_min_station,
        " stations n'ont pas été prises en compte."
      ),
      x = "",
      y = "Proportion",
      fill = "Etat écologique"
    ) +
    ggplot2::theme(
      plot.title = ggtext::element_textbox_simple(
        size = 14,
        face = "bold",
        margin = margin(b = 5)
      ),
      plot.subtitle = ggtext::element_textbox_simple(size = 12, 
                                                     margin = margin(b = 10)),
      legend.title = ggplot2::element_text(size = 10, face = "bold")
    )
  
  data_n <- data_pct %>%
    dplyr::distinct(annee, libelle_indice, n_total)
  
  
  #graphique présentant le nombre total de stations prises en compte par année
  g2 <- ggplot(data_n, aes(x = annee, y = n_total)) +
    ggplot2::geom_col(fill = "grey", color = "black") +
    ggplot2::facet_wrap(~ libelle_indice, ncol = 4) +
    ggplot2::labs(subtitle = " ", x = "Année", y = "Nombre de stations") +
    ggplot2::theme_bw()
  
  #Assembler les deux graphiques
  distrib <- patchwork::wrap_plots(g1, g2, ncol = 1, heights = c(2.5, 1.5))
  
  
  #Télécharger la figure
  ggplot2::ggsave(
    filename = file.path(
      output_dir,
      paste0("proportions_classe_", reseau_titre, ".jpg")
    ),
    plot = distrib,
    width = 14,
    height = 9,
    dpi = 300
  )
}





#' Créer les graphiques présentant l'évolution du nombre de station par classe d'état écologique pour chaque indice au cours du temps.
#'
#' @param data dataframe contenant les résultats
#' @param liste_reseaux liste des réseaux étudiés


barplot_nombre <- function(data, 
                           liste_reseaux, 
                           output_dir = "../Output/4_Proportions_classes_eco/01_Barplot") {
  #Construire automatiquement le "titre réseau"
  reseau_titre <- paste(sort(unique(liste_reseaux)), collapse = "_")
  
  # S'assurer que le répertoire de sortie existe
  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)
  
  #filtrer les données
  data_filtre <- data %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$reseaux %in% liste_reseaux)
  
  #filtrer les données avant 2013 si le réseau est RRP
  if (length(liste_reseaux) == 1 && liste_reseaux == "RRP") {
    data_filtre <- data_filtre %>%
      dplyr::filter(annee >= 2013)
  }
  
  data_counts <- data_filtre %>%
    dplyr::group_by(code_indice, libelle_indice, annee, libelle_classe) %>%
    dplyr::summarise(n_classe = dplyr::n_distinct(code_station),
                     .groups = "drop")
  
  
  data_counts$libelle_classe <- factor(
    data_counts$libelle_classe,
    levels = c("Très bon", "Bon", "Moyen", "Médiocre", "Mauvais"),
    ordered = TRUE
  )
  
  couleurs  = c("red", "orange", "yellow", "lightgreen", "#2b83ba")
  classes   = c("Mauvais", "Médiocre", "Moyen", "Bon", "Très bon")
  palette_etat <- setNames(couleurs, classes)
  
  g3 <- ggplot2::ggplot(data_counts,
                        ggplot2::aes(x = annee, 
                                     y = n_classe, 
                                     fill = libelle_classe)) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::facet_wrap(~ libelle_indice, ncol = 4) +
    scale_fill_manual(
      values = palette_etat,
      breaks = c("Très bon", "Bon", "Moyen", "Médiocre", "Mauvais")
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = paste0(
        "Nombre de stations par classes d'état écologique, pour les stations appartenant au réseaux ",
        reseau_titre,
        " par année."
      ),
      x = "Années",
      y = "Nombre de stations",
      fill = "Etat écologique"
    ) +
    ggplot2::theme(
      plot.title = ggtext::element_textbox_simple(
        size = 14,
        face = "bold",
        margin = margin(b = 5)
      ),
      plot.subtitle = ggtext::element_textbox_simple(size = 12, 
                                                     margin = margin(b = 10)),
      legend.title = ggplot2::element_text(size = 12, face = "bold"),
      axis.title.x = ggplot2::element_text(size = 11),
      axis.title.y = ggplot2::element_text(size = 11)
    )
  
  #Télécharger la figure
  ggplot2::ggsave(
    filename = file.path(
      output_dir,
      paste0("nb_station_classe_", reseau_titre, ".jpg")
    ),
    plot = g3,
    width = 13,
    height = 7,
    dpi = 300
  )
}