#' Générer des graphique boxplot pour présenter l'évolution des métriques de chaque indice
#'
#' @param data dataframe contenant les résultats
#' @param var_code_support variable contenant les codes support
#' @param liste_stations liste contenant les stations étudiées
#' @param var_resultat_metrique variable contenant les résultats à représenter
#' @param var_temps  variable temporelle du modèle (ex:année)


boxplot_metriques <- function(data, var_code_support, liste_stations, 
                              var_resultat_metrique, var_temps) {
  
  # Filtrer les données
  df <- data %>% 
    dplyr::filter(code_support == var_code_support,
           code_station %in% liste_stations)
  
  # Récupérer le libellé de l'indice (unique pour ce support)
  lib_indice <- df %>% 
    dplyr::pull(libelle_indice) %>% 
    unique() %>% 
    dplyr::first()
  
  # Titre automatique
  titre_auto <- paste0("Évolution des métriques utilisées dans le calcul de l'", lib_indice)
  
  # Nom de fichier automatique
  fichier_auto <- paste0("boxplot_metriques_", gsub(" ", "_", lib_indice), ".png")
  
  # Nombre de facettes
  n_facettes <- df %>% 
    dplyr::pull(libelle_metrique) %>% 
    unique() %>% 
    length()
  
  # Choix automatique du nombre de colonnes
  ncol_facettes <- ifelse(n_facettes >= 6, 3, 2)
  
  # Construire le graphique
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[var_temps ]], y = .data[[var_resultat_metrique]])) +
    ggplot2::geom_boxplot(
      ggplot2::aes(group = factor(.data[[var_temps ]])),
                 outlier.colour = "red",
                 fill = "steelblue",
                 alpha = 0.4) +
    ggplot2::facet_wrap(~ libelle_metrique,
               scales = "free_y",
               ncol = ncol_facettes,
               strip.position = "top") +
    ggplot2::scale_x_continuous(
      breaks = seq(min(df %>% dplyr::pull(.data[[var_temps ]])),
                   max(df %>% dplyr::pull(.data[[var_temps ]])),
                   by = 2)
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = titre_auto,
      x = "Années",
      y = "Valeurs des métriques"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 15, face = "bold"),
      axis.title.x = ggplot2::element_text(size = 13, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 13, face = "bold"),
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 12),
      strip.text = ggplot2::element_text(size = 12, face = "bold")
    )
  
  # Sauvegarde
  ggplot2::ggsave(
    filename = fichier_auto,
    plot = p,
    width = 21,
    height = 14,
    dpi = 300,
    path = "../Output/5_Evolution_metriques"
  )
  
  return(p)
}
