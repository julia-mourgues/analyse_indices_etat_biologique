graph_metriques <- function(data, id_station){
  
  # Définir les métriques qui nous interessent
  var_etude <- c("Abondance totale","Richesse specifique", "Diversite de Shannon", "Equitabilite de Pielou")

  # filtrer les données
  data_filtre <- data %>% 
    filter(libelle_metrique %in%var_etude,
           code_station ==id_station,
           libelle_metrique != "Abondance totale" | !(libelle_support =="Diatomées benthiques") )

  # définir une palette
  palette <- c(
    "Macrophytes" = "#00B2A9FF",  
    "Diatomées benthiques"  = "#E43C40FF",  
    "Macroinvertébrés aquatiques" = "#BC7844FF",  
    "Poissons" = "#8942BDFF"  
  )

  
  # Informations sur la station
  libelle_station <- unique(data_filtre$libelle_station)
  code_station    <- unique(data_filtre$code_station)
  code_departement <- unique(data_filtre$code_departement)
  
  
  
  # fonction interne pour un graphique
  plot_communaute <- function(df, nom_support){
    
    ggplot(df, aes(x = annee, y = resultat_metrique)) +
      geom_point(aes(colour = libelle_support)) +
      geom_smooth(method = "lm", se = TRUE, alpha = 0.1,
                  aes(color = libelle_support, fill = libelle_support)) +
      scale_colour_manual(values = palette) +
      scale_fill_manual(values = palette) +
      guides(fill = "none", colour="none") +
      scale_x_continuous(
        breaks = seq(min(df$annee), max(df$annee), by = 2)
      ) +
      scale_y_continuous(limits = c(0, NA)) +
      facet_wrap(~libelle_metrique, scales = "free_y", ncol = 4) +
      labs(
        subtitle = paste("Communauté :", nom_support),
        x = NULL,
        y = NULL
      ) +
      theme_bw() +
      theme(
        plot.subtitle = ggtext::element_textbox_simple(margin=margin(t=12,b=10), size = 14, face = "bold"),
        axis.text.x = element_text(size=12,angle = 45, hjust = 1),
        axis.text.y=element_text(size=12),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing = unit(1, "lines")
      )
  }
  
  # créer les plots automatiquement
  liste_supports <- c("Poissons", "Macrophytes", 
                      "Macroinvertébrés aquatiques", 
                      "Diatomées benthiques")
  
  plots <- lapply(liste_supports, function(supp){
    df <- data_filtre %>% filter(libelle_support == supp)
    if(nrow(df) == 0) return(NULL)
    plot_communaute(df, supp)
  })
  
  # enlever les NULL
  plots <- plots[!sapply(plots, is.null)]
  
  # combiner les graphiques
  plot_combine <-  patchwork::wrap_plots(plots, ncol = 1) +
    patchwork::plot_annotation(
      title = "Tendance temporelle des métriques de diversité par communauté biologique",
      theme = theme(plot.title = ggtext::element_textbox_simple(size = 14, face = "bold", margin=margin(t=10,b=10)))
    ) 
  return(plot_combine)
}

