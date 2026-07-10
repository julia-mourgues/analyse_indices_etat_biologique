#' Fonction permettant de générer le graphique facetté présentant les tendances temporelles des indices
#' pour la station étudiée

graph_lmm <- function(data_indice,
                      tendances_RCS,
                      tendances_RRP,
                      tendances_stations,
                      tab_valeurs_seuils,
                      id_station){
  
  # Préparation classes pour l'arriere plan
  classes <- tab_valeurs_seuils %>%
    dplyr::rename(
      classe_borne_inf = seuil_bas,
      classe_borne_sup = seuil_haut,
      classe_libelle   = classe,
      classe_couleur   = couleur
    )
  
  #Ordonner les niveaux des classes d'état 
  classes$classe_couleur <- lighten(classes$classe_couleur, 0.7)
  classes$classe_libelle <- factor(
    classes$classe_libelle,
    levels = c("Mauvais", "Médiocre", "Moyen", "Bon", "Très bon")
  )
  
  # Déterminer la couleur des tendances
  reseaux_libelle <- c("Réseau RRP", "Réseau RCS", "Station")
  couleurs <- c("#2D0084", "#CD6839", "#990099FF")
  
  names(couleurs) <- reseaux_libelle
  
  # Filtrer les données en fonction de la station étudiée
  data2<- data_indice %>% 
    filter(code_station==id_station)%>% 
    mutate(type_tendance = "Station")
  
  
  if (nrow(data2) == 0) {
    stop(paste("Aucune donnée pour la station", id_station))
  }
  
  #Déterminer l'appartenance réseau de la station
  reseau_station <- unique(data2$reseaux)[1]

  
  valeurs_station_2 <- tendances_stations%>% 
    filter(code_station==id_station)%>% 
    mutate(type_tendance = "Station")
  
  
  classes_filtre <- classes %>% 
    filter(CODE_STATION==id_station) %>% 
    rename(libelle_indice=indice)
  
  # Sélection du bon jeu de données selon le réseau 
  
  valeurs_lmm <- if (reseau_station == "RRP") {
    tendances_RRP
  } else {
    tendances_RCS
  }
  
  valeurs_lmm2 <- valeurs_lmm %>%
    mutate(type_tendance = paste0("Réseau ", reseau_station))
  
  if (nrow(valeurs_lmm2) == 0) {
    stop(paste("Aucune donnée de réseaux pour la station", id_station))
  }
  
  # objets pour le graphique
  libelle_station <- unique(data2$libelle_station)
  code_station    <- unique(data2$code_station)
  code_departement <- unique(data2$code_departement)
  libelle_reseau <- paste("Réseau ",unique(data2$reseaux))
  
  # Graphique
  plot <- ggplot(data2, aes(x = annee, y = resultat_eqr)) +
    #couleur des classes d'état biologiques
    geom_rect( 
      data = classes_filtre,
      aes(
        ymin = classe_borne_inf,
        ymax = classe_borne_sup,
        fill = classe_libelle
      ),
      xmin = -Inf,
      xmax = Inf,
      alpha = 0.7,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(
      name = "État biologique",
      values = setNames(classes_filtre$classe_couleur, 
                        classes_filtre$classe_libelle),
      guide = guide_legend(reverse = TRUE)
    ) +
    #limite inférieure de l'intervalle de confiance associé à la tendance du réseau
    geom_line(
      data = valeurs_lmm2 %>% 
        filter(libelle_indice %in% unique(data2$libelle_indice)),
      aes(x = annee, y = lower, colour = type_tendance,),
      linetype = "dotted",
      linewidth = 0.9,
      alpha=0.7,
      inherit.aes = FALSE
    ) +
    #limite supérieure de l'intervalle de confiance associé à la tendance du réseau
    geom_line(
      data = valeurs_lmm2 %>% 
        filter(libelle_indice %in% unique(data2$libelle_indice)),
      aes(x = annee, y = upper,
          colour = type_tendance),
      linetype = "dotted",
      linewidth = 0.9,
      alpha=0.7,
      inherit.aes = FALSE
    )+
    #tendance du réseau
    geom_line(
      data = valeurs_lmm2 %>% 
        filter(libelle_indice %in% unique(data2$libelle_indice)),
      aes(x = annee, y = fit, colour = type_tendance),
      linewidth = 1.2,
      linetype = 2,
      inherit.aes = FALSE
    ) +
    #valeurs brutes des indices
    geom_point(aes(colour = type_tendance), 
               size = 4,
               alpha = 0.7) +
    #tendance de la station
    geom_line(
      data = valeurs_station_2,
      aes(x = annee, 
          y = fit, 
          colour = type_tendance),
      linewidth = 1.2
    ) +
    scale_color_manual(
      name = "Tendance",
      values = couleurs)+
    scale_x_continuous(
      breaks = seq(
        from = min(data2$annee),
        to   = max(data2$annee),
        by   = 2
      ))+
    facet_wrap(~ libelle_indice, 
               scales = "free_y",
               labeller = as_labeller(c(
                 "IPR" = "Indice Poisson",
                 "IBMR" = "Indice Macrophytes",
                 "IBD" = "Indice Diatomées",
                 "I2M2" = "Indice Macroinvertébrés"
               ))
    ) +
    facetted_pos_scales(
      y = list(
        libelle_indice == "IPR" ~ scale_y_reverse(), #inverser l'axe des y pour l'IPR 
        TRUE ~ scale_y_continuous()
      )
    )+
    labs(
      x = "Année",
      y = "Résultat indice"
    ) +
    theme_bw() +
    theme(
          axis.title = element_text(size=16, face="bold"),
          axis.text = element_text(size=15),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title=element_text(size=15, face="bold"),
          legend.text=element_text(size=14),
          legend.position = "right",
          legend.title.position = "top",
          strip.text = element_text(size=15, face="bold")
    )
  return(plot)
}

