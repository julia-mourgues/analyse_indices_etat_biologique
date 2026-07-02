#' Fonction permettant de générer le tableau présentant l'évolution de l'état
#' biologique de la station au cours du temps, au regard des 4 indices étudiés
#' mais aussi l'état biologique global de la station et l'indice déclassant associé

tab_element_declassant <- function (data, id_station) {
  #Définir les classes d'état biologique
  classe_libelle <- c("Mauvais", "Médiocre", "Moyen", "Bon", "Très bon")
  couleurs <- c("#FF0000", "#FFA500", "#FFFF00", "#90EE90", "#2b83ba")
  
  names(couleurs) <- classe_libelle
  
  #filtrer les données
  data2 <- data %>%
    filter(code_station == id_station) %>%
    mutate(element_declassant = sapply(element_declassant, function(x)
      paste(x, collapse = ", "))) %>%
    dplyr::select(
      code_departement,
      reseaux,
      libelle_station ,
      annee,
      IBD,
      IBMR,
      I2M2,
      IPR,
      etat_biologique,
      element_declassant
    ) %>%
    arrange(desc(annee)) %>%
    mutate(etat_biologique = as.character(etat_biologique))
  
  #mise en forme conditionnelle des cellules
  color_cell <- function(x) {
    couleurs[x]
  }
  
  # objets pour le tableau
  legende <- paste0("<span style='color:",
                    couleurs,
                    ";'>■</span> ",
                    names(couleurs),
                    collapse = "&nbsp;&nbsp;")
  
  libelle_station <- data2 %>%
    dplyr::distinct(libelle_station) %>%
    dplyr::pull()
  
  code_departement <- data2 %>%
    dplyr::distinct(code_departement) %>%
    dplyr::pull()
  
  
  #Création du tableau
  table_data <- data2 %>%
    gt() %>%
    cols_hide(columns = c(code_departement, reseaux, libelle_station)) %>%
    tab_options(
      table.width = pct(70),
      table.font.size = px(13),
      data_row.padding = px(5)
    ) %>%
    tab_style(style = list(
      cell_text(indent = px(20)),
      cell_borders(
        sides = "all",
        color = "#f7f7f7",
        weight = px(2)
      )
    ),
    locations = cells_body()) %>%
    tab_style(style = cell_text(size = px(13), weight = "bold"),
              locations = cells_column_labels()) %>%
    tab_style(style = cell_text(size = px(12)),
              locations = cells_title(groups = "subtitle")) %>%
    opt_row_striping() %>%
    cols_label(
      libelle_station  = "Station",
      etat_biologique  = "Etat biologique",
      reseaux = "Réseau",
      annee = "Année",
      element_declassant = "Elément déclassant"
    ) %>%
    tab_header(title = md(
      glue(
        "Classe d'état biologique des 4 indices par année, et classe d'état biologique retenue pour la DCE pour la station **{libelle_station}** ({id_station} - Département {code_departement})."
      )
    )) %>%
    sub_missing(columns = c("IBD", "IBMR", "IPR", "I2M2")) %>%
    
    # Coloration des indices
    data_color(
      columns = c("IBD", "IBMR", "I2M2", "IPR"),
      fn = function(x) {
        col <- couleurs[as.character(x)]
        col[is.na(col)] <- "#FFFFFF"
        col
      }
    ) %>%
    
    # Coloration de l'état biologique
    data_color(
      columns = "etat_biologique",
      fn = function(x) {
        col <- couleurs[as.character(x)]
        col[is.na(col)] <- "#FFFFFF"
        col
      }
    ) %>%
    
    # Remplacer le texte de la colonne etat_biologique
    text_transform(
      locations = cells_body(columns = "etat_biologique"),
      fn = function(x) {
        " "
      }
    ) %>%
    
    # Supprimer le texte dans les indices
    text_transform(
      locations = cells_body(columns = c("IBD", "IBMR", "IPR", "I2M2")),
      fn = function(x) {
        " "
      }
    ) %>%
    #définir la largeur des colonnes
    cols_width(
      c(IBD, IBMR, I2M2, IPR) ~ px(60),
      etat_biologique ~ px(60),
      element_declassant ~ px(90),
      annee ~ px(45)
    ) %>%
    
    # Centrer le texte de etat_biologique
    cols_align(align = "center",
               columns = c("etat_biologique", "element_declassant")) %>%
    tab_source_note(source_note = md(legende)) %>%
    tab_style(style = cell_text(size = px(13)), locations = cells_source_notes()) %>%
    #renommer les colonnes
    cols_label(
      IBD = "Indice Diatomées",
      IBMR = "Indice Macrophytes",
      IPR = "Indice Poissons",
      I2M2 = "Indice Macroinvertébrés"
    )
  
  return(table_data)
}
