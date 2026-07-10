#' Fonction permettant de générer un graphique en barre pour présenter l'occupation du sol autour de la station
#'
landuse <- function(data, id_station) {
  #filtre sur la station d'intéret
  data_filtre <- data %>%
    st_drop_geometry() %>%
    filter(sta_code_sandre == id_station) %>%
    select(categorie, pct)
  
  #Création graphique
  graph <- ggplot2::ggplot(data_filtre,
                           ggplot2::aes(x = categorie, 
                                        y = pct, 
                                        fill = categorie)) +
    ggplot2::geom_col() +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "", y = "Proportion (%)") +
    guides(fill = "none") +
    ggplot2::theme(
      axis.title = element_text(size = 13, face = "bold"),
      axis.text = element_text(size = 12, face = "bold")
    )
  return(graph)
}