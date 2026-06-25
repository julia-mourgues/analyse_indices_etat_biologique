#' Créer les graphiques par département présentant la disponibilité des données exportées via aspe et Naïades
#'
#' @param data_aspe dataframe contenant les données aspe
#' @param data_hydrobio dataframe contenant les données hydrobio
#' @param departement numéro du département qui à représenter
#' @param repertoire chemin d'accès du dossier dans lequel sauvegarder le graphique


dispo_indices <- function(data_aspe,
                          data_hydrobio,
                          departement,
                          nom_fichier,
                          col_libelle_qualification,
                          repertoire = ".") {
  data_aspe_filtre <- data_aspe %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$dept == departement) %>%
    sf::st_drop_geometry() %>%
    dplyr::distinct(annee, sta_libelle_sandre, obj_libelle)  # supprime les doublons
  
  data_hydrobio_filtre <- data_hydrobio %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$code_departement == departement) %>%
    sf::st_drop_geometry()
  
  # Détection des conflits (plusieurs libellés pour une même tuile)
  data_split <- data_aspe_filtre %>%
    dplyr::group_by(annee, sta_libelle_sandre) %>%
    dplyr::mutate(
      n = dplyr::n_distinct(obj_libelle),
      # nombre réel de libellés
      rang = dplyr::dense_rank(obj_libelle)     # rang basé sur les libellés distincts
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      y_num = as.numeric(factor(sta_libelle_sandre)),
      xmin = annee - 0.5,
      xmax = annee + 0.5,
      ymin = y_num - 0.5 + (rang - 1) / n,
      ymax = y_num - 0.5 + rang / n
    )
  
  p <- ggplot2::ggplot(data_split, aes(x = annee, y = as.character(sta_libelle_sandre))) +
    
    ggplot2::geom_rect(aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = obj_libelle
    ),
    alpha=0.4,
    color = NA) +
    geom_hline(
      yintercept = unique(data_split$y_num) - 0.5,
      color = "black",
      linewidth = 0.4
    ) +
    scale_fill_manual(
      name = "Réseaux de pêche",
      values = c(
        "RRP" = "#2D0084",
        "RCS" = "#CD6839",
        "RHP"="#009E73FF"
      ))+
    ggplot2::geom_point(
      data = data_hydrobio_filtre,
      aes(
        x = annee,
        y = as.character(libelle_station_hydrobio),
        shape = libelle_indice,
        color = .data[[col_libelle_qualification]]
      ),
      size = 3.5,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        "IBD" = 1,
        # cercle
        "IBMR" = 3,
        # plus
        "I2M2" = 4 # croix
      )) + 
    ggplot2::scale_color_manual(values = c(
      "correcte" = "black",
      "non définissable" = "red")) +
    ggplot2::labs(
      title = "Disponibilité des données Aspe et hydrobio par années en fonction du département.",
      subtitle = paste0("Département ", departement),
      x = "Années",
      y = "",
      fill = "Réseaux de pêche",
      shape = "Indice d'état biologique",
      color="Qualification des données"
    ) +
    theme_minimal() +
    ggplot2::theme(
      plot.title = element_textbox_simple(
        size = 12,
        face = "bold",
        hjust = 0,
        vjust = 1
      ),
      plot.subtitle = element_text(size = 11, vjust = 0),
      axis.title.x = element_text(size = 10),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 9),
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 7),
      legend.position = "bottom",
      legend.title.position = "top"
    ) +
    ggplot2::guides(
      fill  = guide_legend(order = 1, ncol = 1),
      # légende des tuiles en premier
      shape = guide_legend(order = 2, ncol = 1),    # légende des points en dessous
      color = guide_legend(order = 3, ncol = 1)
    )
  
  # Construction du chemin complet
  fichier_dispo <- file.path(repertoire,
                             paste0("dispo_donnees_", nom_fichier, "_", departement, ".jpg"))
  
  # --- Sauvegarde automatique en JPG ---
  ggplot2::ggsave(
    filename = fichier_dispo,
    plot = p,
    width = 10,
    height = 7,
    dpi = 300
  )
  return(p)
}
