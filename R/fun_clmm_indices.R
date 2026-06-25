#' Faire tourner des modèles mixtes de liens cumulatifs pour chaque indice
#'
#' @param data dataframe contenant les données
#' @param nom_indice une liste contenant les libelle des indices étudiés


clmm_indices <- function(data, 
                         nom_indice,
                         formule_modele){
  

  data_filtre <- data%>% 
    dplyr::filter(libelle_indice==nom_indice)
  
  # création d'un dossier pour chaque indice
  dossier <- paste0("../Output/4_Proportions_classes_eco/02_Resultats_CLMM/Indice_", nom_indice)
  
  if (!dir.exists(dossier)) {
    dir.create(dossier, recursive = TRUE)
  }
  
  #Faire tourner le modele séparément pour chaque réseaux
  reseaux_uniques <- unique(data_filtre$reseaux)
  resultats_par_reseau <- vector("list", length(reseaux_uniques))
  names(resultats_par_reseau) <- reseaux_uniques
  
  for (r in reseaux_uniques) {
    
    df_r <- data_filtre %>%
      dplyr::filter(.data$reseaux == r)
    
    if (nrow(df_r) == 0) {
      warning("Aucune donnée pour le réseau : ", r, " (indice ", nom_indice, ")")
      next
    }
    
    # Définir le modèle mixte
    modele_mixte <- ordinal::clmm(formule_modele, data = df_r)
    
    summary(modele_mixte)
    
    formule_txt <- rlang::expr_text(formule_modele) #servira pour les titres
    
    
    # récupérer la table contenant les effets fixes
    effets_fixes<- broom.mixed::tidy(modele_mixte) %>%
      dplyr::mutate(
        OR = exp(.data$estimate),             # odds ratios
        term = factor(.data$term, levels = .data$term),  # ordre figé
        indice=nom_indice,
        reseaux=r
      )
    
    # Sauvegarde de la table OR
    readr::write_csv(
      effets_fixes,
      file.path(dossier, paste0("effets_fixes_OR_",r,".csv"))
    )
    
    plot_effets_fixes <- ggplot2::ggplot(effets_fixes, 
                                         aes(x = .data$term, y = .data$estimate)) +
      ggplot2::geom_point(size = 3, color = "#1f78b4") +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$estimate - .data$std.error,
                        ymax = .data$estimate + .data$std.error),
                    width = 0.2, color = "#1f78b4") +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
      ggplot2::coord_flip() +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::labs(
        title = paste0("Effets fixes du modèle ordinal mixte pour le réseau ",r,"- Indice ", nom_indice),
        subtitle=paste0("Modele= clmm(",formule_txt,",data = df_r)"),
        y = "Coefficient (log-odds)",
        x = "Variable explicative"
      )+
      ggplot2::theme(plot.title=ggtext::element_textbox_simple(size=14, 
                                              face="bold", 
                                              margin = margin(b = 10)),
            plot.subtitle=ggtext::element_textbox_simple(size=13, 
                                                         margin = margin(b = 10)))
    
    
  
    
    # Sauvegarde du graphique des effets fixes
    ggplot2::ggsave(
      filename = file.path(dossier, paste0("effets_fixes_",r,".jpg")),
      plot = plot_effets_fixes,
      width = 10, height = 7, dpi = 300
    )
    
    #Effets aléatoires pour tous les départements
    ranef_station_df <- ordinal::ranef(modele_mixte)$code_station %>%
      as.data.frame() %>%
      tibble::rownames_to_column("code_station") %>%
      dplyr::left_join(
        df_r %>%
          dplyr::select(.data$code_station,
                        .data$libelle_station,
                        .data$code_departement) %>%
          dplyr::distinct(),
        by = "code_station"
      )
    
    ranef_station_df_ordre <- ranef_station_df %>%
      dplyr::arrange(.data$`(Intercept)`) %>%
      dplyr::mutate(
        libelle_station = factor(.data$libelle_station,
                                 levels = .data$libelle_station)
      )
    
    # PDF des effets aléatoires (toutes stations du réseau)
    grDevices::pdf(
      file.path(dossier, paste0("effets_aleatoires_", r, ".pdf")),
      width = 10, height = 20
    )
    
    print(
      ggplot2::ggplot(ranef_station_df_ordre,
                      ggplot2::aes(x = .data$libelle_station,
                                   y = .data$`(Intercept)`)) +
        ggplot2::geom_point(color = "#2b83ba", size = 2) +
        ggplot2::geom_segment(
          ggplot2::aes(
            xend = .data$libelle_station,
            y = 0,
            yend = .data$`(Intercept)`
          ),
          color = "grey70"
        ) +
        ggplot2::geom_hline(
          yintercept = 0,
          linetype = "dashed",
          color = "grey40"
        ) +
        ggplot2::coord_flip() +
        ggplot2::theme_bw() +
        ggplot2::labs(
          title = paste0("Effets aléatoires — Réseau ", r, " — Indice ", nom_indice),
          subtitle = paste0("Modèle : clmm(", formule_txt, ", data = df_r)"),
          y = "Effet aléatoire (intercept)",
          x = "Station"
        ) +
        ggplot2::theme(
          plot.title = ggtext::element_textbox_simple(
            size = 13, face = "bold", margin = margin(b = 10)
          ),
          plot.subtitle = ggtext::element_textbox_simple(
            size = 11, margin = margin(b = 10)
          )
        )
    )
    
    grDevices::dev.off()
    
    # Stocker les résultats pour ce réseau
    resultats_par_reseau[[as.character(r)]] <- list(
      reseaux           = r,
      modele            = modele_mixte,
      effets_fixes      = effets_fixes,
      effets_fixes_OR   = effets_fixes,  # déjà avec OR
      plot_effets_fixes = plot_effets_fixes,
      effets_aleatoires = ranef_station_df,
      dossier           = dossier
    )
  }
  
  # 5. Retourner la liste complète
  return(resultats_par_reseau)
 
}
