#' Créer les graphiques modélisant l'évolution des valeurs de chaque indice, avec ajout d'une droite de régression

bornes_indices <- list(
  #bornes pour l'I2M2
  "7613" = list( 
    borne_inf = c(0,0.148,0.295,0.443,0.665),
    borne_sup = c(0.148,0.295,0.443,0.665,1),
    couleurs  = c("red","orange","yellow","lightgreen","#2b83ba"),
    classe_libelle = c("Mauvais", "Médiocre", "Moyen", "Bon", "Très bon")
  ),
  #bornes pour l'IBD
  "5856" = list(
    borne_inf = c(0,0.30,0.55,0.78,0.94),
    borne_sup = c(0.30,0.55,0.78,0.94,1),
    couleurs  = c("red","orange","yellow","lightgreen","#2b83ba"),
    classe_libelle = c("Mauvais", "Médiocre", "Moyen", "Bon", "Très bon")
  ),
  #bornes pour l'IBMR
  "2928"=list(
    borne_inf = c(0,0.51,0.64,0.77,0.92),
    borne_sup = c(0.51,0.64,0.77,0.92,1),
    couleurs  = c("red","orange","yellow","lightgreen","#2b83ba"),
    classe_libelle = c("Mauvais", "Médiocre", "Moyen", "Bon", "Très bon")
  ),
  #bornes pour l'IPR
  "7036"=list(
    borne_inf = c(0,5,16,25,36),
    borne_sup = c(5,16,25,36,60),
    couleurs  = c("red","orange","yellow","lightgreen","#2b83ba"),
    classe_libelle = c("Mauvais", "Médiocre", "Moyen", "Bon", "Très bon")
  )
)
#' @param data dataframe contenant les résultats
#' @param code_indice_val code de l'indice que l'on veut représenter
#' @param bornes_indices  listes contenant les bornes inférieures et supérieures des classes de chaque indice
#' @param var_temporelle  variable temporelle du modèle (ex:année)
#' @param col_resultat_indice variable contenant les résultats à représenter

source(here::here("R","fun_classe_ecologique.R"))

trend_indice <- function(data,
                         code_indice_val,
                         bornes_indices,
                         sous_titre,
                         repertoire = ".",
                         var_temporelle,
                         col_resultat_indice,
                         col_libelle_qualification) {
  classes <- creation_classes(code_indice_val, bornes_indices)
  
  data_filtre <- data %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$code_indice == code_indice_val)
  
  if ("sf" %in% class(data_filtre)) {
    data_filtre <- sf::st_drop_geometry(data_filtre)
  }
  
  message("Aperçu des données utilisées :")
  print(
    data_filtre %>%
      dplyr::summarise(
        n = n(),
        min_val = min(.data[[col_resultat_indice]], na.rm = TRUE),
        max_val = max(.data[[col_resultat_indice]], na.rm = TRUE),
        indices = paste(unique(code_indice), collapse = ", ")
      )
  )
  
  libelle_indice <- data_filtre %>%
    dplyr::distinct(libelle_indice) %>%
    dplyr::pull()
  
  p <- ggplot2::ggplot(data_filtre, ggplot2::aes(x = .data[[var_temporelle]], y = .data[[col_resultat_indice]])) +
    ggplot2::geom_rect(
      data = classes,
      ggplot2::aes(
        ymin = classe_borne_inf,
        ymax = classe_borne_sup,
        fill = classe_libelle
      ),
      xmin = -Inf,
      xmax = Inf,
      alpha = 0.3,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(
      name = "Etat écologique",
      values = setNames(classes$classe_couleur, classes$classe_libelle),
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = .data[[col_libelle_qualification]], color = .data[[col_libelle_qualification]]),
      size = 3
    ) +
    ggplot2::scale_shape_manual(values = c(
      "correcte" = 20,
      # cercle petit plein
      "non définissable" = 4 # croix
    )) +
    # Droite globale
    ggplot2::geom_smooth(
      ggplot2::aes(color = "Toutes les données", linetype = "Toutes les données"),
      method = "lm",
      se = TRUE,
      alpha = 0.25
    ) +
    
    # Droite correcte uniquement
    ggplot2::geom_smooth(
      data = dplyr::filter(data_filtre, .data[[col_libelle_qualification]] == "correcte"),
      ggplot2::aes(color = "Données correctes uniquement", linetype = "Données correctes uniquement"),
      method = "lm",
      se = TRUE,
      alpha = 0.4
    ) +
    ggplot2::scale_color_manual(
      name = "Type de régression",
      values = c(
        "Toutes les données" = "blue",
        "Données correctes uniquement" = "red"
      )
    ) +
    ggplot2::scale_linetype_manual(
      name = "Type de régression",
      values = c(
        "Toutes les données" = "dashed",
        "Données correctes uniquement" = "solid"
      )
    ) +
    ggplot2::scale_x_continuous(breaks = seq(
      min(data_filtre[[var_temporelle]], na.rm = TRUE),
      max(data_filtre[[var_temporelle]], na.rm = TRUE),
      by = 2
    )) +
    ggplot2::labs(
      title = paste0("Évolution de l'", libelle_indice, " en fonction des années"),
      subtitle = sous_titre,
      x = "Année",
      y = "Valeur de la métrique",
      fill = "Etat écologique",
      shape = "Qualification des données"
    ) +
    ggplot2::theme_bw()
  
  if (code_indice_val == 7036) {
    p <- p + ggplot2::scale_y_reverse()
  }
  
  fichier_tendance <- file.path(repertoire, paste0("tendance_lm_", code_indice_val, ".jpg"))
  
  ggplot2::ggsave(
    filename = fichier_tendance,
    plot = p,
    width = 10,
    height = 7,
    dpi = 300
  )
  
  return(p)
}





boxplot_indice_DCE <- function(data,
                               code_indice_val,
                               bornes_indices,
                               sous_titre,
                               repertoire = ".",
                               var_group,
                               col_resultat_indice) {
  classes <- creation_classes(code_indice_val, bornes_indices)
  
  data_filtre <- data %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$code_indice == code_indice_val)
  
  if ("sf" %in% class(data_filtre)) {
    data_filtre <- sf::st_drop_geometry(data_filtre)
  }
  
  nb_stations_total <- data_filtre %>%
    dplyr::summarise(nb = dplyr::n_distinct(code_station)) %>%
    dplyr::pull(nb)
  
  sous_titre_complet <- paste0(sous_titre, " — ", nb_stations_total, " stations utilisées")
  
  
  message("Aperçu des données utilisées :")
  print(
    data_filtre %>%
      dplyr::summarise(
        n = n(),
        min_val = min(.data[[col_resultat_indice]], na.rm = TRUE),
        max_val = max(.data[[col_resultat_indice]], na.rm = TRUE),
        indices = paste(unique(code_indice), collapse = ", ")
      )
  )
  
  nb_stations <- data_filtre %>%
    dplyr::group_by(.data[[var_group]]) %>%
    dplyr::summarise(nb_stations = dplyr::n_distinct(code_station)) %>%
    dplyr::arrange(.data[[var_group]])
  
  message("Nombre de stations par cycle DCE :")
  print(nb_stations)
  
  libelle_indice <- data_filtre %>%
    dplyr::distinct(libelle_indice) %>%
    dplyr::pull()
  
  p <- ggplot2::ggplot(data_filtre, ggplot2::aes(x = factor(.data[[var_group]]), y = .data[[col_resultat_indice]])) +
    ggplot2::geom_rect(
      data = classes,
      ggplot2::aes(
        ymin = classe_borne_inf,
        ymax = classe_borne_sup,
        fill = classe_libelle
      ),
      xmin = -Inf,
      xmax = Inf,
      alpha = 0.3,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(
      name = "Etat écologique",
      values = setNames(classes$classe_couleur, classes$classe_libelle),
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggdist::stat_halfeye(
      slab_fill = "black",
      alpha=0.15,
      adjust = 0.5,
      width = 0.6,
      justification = -0.05,
      .width = 0,
      point_colour = NA
    ) +
    ggplot2::geom_boxplot(
      width = 0.2,
      outlier.shape = NA,
      position = position_nudge(x = 0),
      notch = TRUE
    ) +
    ggplot2::labs(
      title = paste0(
        "Distribution des médianes de l'",
        libelle_indice,
        " en fonction des cycles DCE."
      ),
      subtitle = sous_titre_complet,
      x = "Année",
      y = "Valeur médiane",
      fill = "Etat écologique"
    ) +
    ggplot2::theme_bw() +
    theme(
      plot.margin = margin(
        t = 20,
        r = 15,
        b = 15,
        l = 15
      ),
      plot.title = ggtext::element_textbox_simple(margin = margin(b = 10)),
      plot.subtitle = element_text(margin = margin(b = 15))
    )
  
  if (code_indice_val == 7036) {
    p <- p + ggplot2::scale_y_reverse()
  }
  
  
  fichier_boxplot <- file.path(repertoire, paste0("boxplot_dce_", code_indice_val, ".jpg"))
  
  ggplot2::ggsave(
    filename = fichier_boxplot,
    plot = p,
    width = 10,
    height = 7,
    dpi = 300
  )
  
  return(p)
}
