landuse <- function(data,
                    id_station){
  
  data_filtre <- data %>% 
    st_drop_geometry() %>% 
    filter(sta_code_sandre==id_station) %>% 
    select(categorie,
           pct)
  
  
 graph <- ggplot2::ggplot(data_filtre, ggplot2::aes(x = categorie, y = pct, fill = categorie)) +
    ggplot2::geom_col() +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = 
        "Proportion des types d'occupation des sols dans un périmetre de 2,5km autour de la station.",
      x = "",
      y = "Proportion"
    ) +
    guides(fill="none")+
    ggplot2::theme(
      plot.title = ggtext::element_textbox_simple(
        size = 12,
        face = "bold",
        margin = margin(t=10,b = 10)
      ),
      axis.title = element_text(size=10,
                                face="bold"),
      axis.text.x = element_text(size=9,
                                 face="bold")
    )
 return(graph)
}