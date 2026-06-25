
graph_lmm <- function(data_indice,
                      tendances_RCS,
                      tendances_RRP,
                      tendances_stations,
                      id_station){
  
  # Préparation classes pour l'arriere plan
  classes <- valeurs_seuils %>%
    dplyr::rename(
      classe_borne_inf = seuil_bas,
      classe_borne_sup = seuil_haut,
      classe_libelle   = classe,
      classe_couleur   = couleur
    )
  
  classes$classe_couleur <- lighten(classes$classe_couleur, 0.7)
  classes$classe_libelle <- factor(
    classes$classe_libelle,
    levels = c("Mauvais", "Médiocre", "Moyen", "Bon", "Très bon")
  )
  
  # Déterminer la couleur des tendances
  reseaux_libelle <- c("Réseau RRP", "Réseau RCS", "Station")
  couleurs <- c("#2D0084", "#CD6839", "#990099FF")
  
  names(couleurs) <- reseaux_libelle
  
  # Données stations
  data2<- data_indice %>% 
    filter(code_station==id_station)%>% 
    mutate(type_tendance = "Station")
  
  
  if (nrow(data2) == 0) {
    stop(paste("Aucune donnée pour la station", id_station))
  }
  
  
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
  
  # objets pour le graph
  libelle_station <- unique(data2$libelle_station)
  code_station    <- unique(data2$code_station)
  code_departement <- unique(data2$code_departement)
  libelle_reseau <- paste("Réseau ",unique(data2$reseaux))
  
  # Graphique
  plot <- ggplot(data2, aes(x = annee, y = resultat_eqr)) +
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
      values = setNames(classes_filtre$classe_couleur, classes_filtre$classe_libelle),
      guide = guide_legend(reverse = TRUE)
    ) +
    geom_line(
      data = valeurs_lmm2 %>% 
        filter(libelle_indice %in% unique(data2$libelle_indice)),
      aes(x = annee, y = lower, colour = type_tendance,),
      linetype = "dotted",
      linewidth = 0.9,
      alpha=0.7,
      inherit.aes = FALSE
    ) +
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
    geom_line(
      data = valeurs_lmm2 %>% 
        filter(libelle_indice %in% unique(data2$libelle_indice)),
      aes(x = annee, y = fit, colour = type_tendance),
      linewidth = 1.2,
      linetype = 2,
      inherit.aes = FALSE
    ) +
    geom_point(aes(colour = type_tendance), 
               size = 4,
               alpha = 0.7) +
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
    facet_wrap(~ libelle_indice, scales = "free_y") +
    facetted_pos_scales(
      y = list(
        libelle_indice == "IPR" ~ scale_y_reverse(),
        TRUE ~ scale_y_continuous()
      )
    )+
    labs(
      caption="**I2M2**= Indice Invertébrés Multimétrique; **IBD**= Indice Biologique Diatomées; **IBMR**= Indice Biologique Macrophytes en Rivière; **IPR**= Indice Poisson Rivière. ",
      x = "Année",
      y = "Résultat indice"
    ) +
    theme_bw() +
    theme(
          axis.title = element_text(size=15, face="bold"),
          axis.text = element_text(size=14),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title=element_text(size=14, face="bold"),
          legend.text=element_text(size=13),
          legend.position = "right",
          legend.title.position = "top",
          strip.text = element_text(size=14, face="bold"),
          plot.caption = element_textbox_simple(
            size = 13,
            hjust = 0,      
            lineheight = 1.2,
            margin=margin(t=10, b=5),
            fill = "white", 
            box.color = "black",
            width = unit(1, "npc")
          )
    )
  return(plot)
}

