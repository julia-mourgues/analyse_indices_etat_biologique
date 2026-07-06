#' Ce script permet de générer les fiches opérationnelles pour chaque station.
#' Il permet de les générer aux formats pdf, html et docx selon la partie du code exécutée.
#' Il suffit de mettre à jour les tables d'entrée dans les fichiers 080_fiches_stations_pdf.Rmd et / ou 080_fiches_stations_html.Rmd.

# lecture des stations
stations_etude <- read_delim("Output/stations_etude.csv",
                             delim = ";",
                             show_col_types = FALSE) #%>% 
  # filter(sta_code_sandre == "03264000")  #à décommenter si besoin, pour choisirles stations étudiées

# création des dossier de sortie pour les différents formats
dir.create(
  "Output/8_Fiches_stations/Fiches-stations-pdf",
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  "Output/8_Fiches_stations/Fiches-stations-html",
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  "Output/8_Fiches_stations/Fiches-stations-word",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "Script/figures", 
  showWarnings = FALSE) #permet de stocker les fichiers temporaires


# Commenter / Décommenter les parties selon le format voulu

#-------------------------------------------------------------------------------
# Format PDF
#-------------------------------------------------------------------------------
# for (i in seq_len(nrow(stations_etude))) {
#   id <- stations_etude$sta_code_sandre[i]
#   nom <- gsub("'", "’", stations_etude$sta_libelle_sandre[i])
#   
#   
#   
#   message("Génération de la fiche ",
#           i,
#           "/",
#           nrow(stations_etude),
#           " - Station ",
#           id,
#           " (",
#           nom,
#           ")")
#   
#   rmarkdown::render(
#     here::here("R/fiches_stations", "080_Fiches_stations_pdf.Rmd"),
#     #Template
#     output_format = "pdf_document",
#     output_dir = "Output/8_Fiches_stations/Fiches-stations-pdf",
#     #Fichier de sortie
#     output_file =
#       paste0("Fiche-station-", id, ".pdf"),
#     params = list(
#       code_station = id,
#       libelle_station = nom,
#       auteur = "OFB DR Normandie"
#     ),
#     clean = TRUE,
#     quiet = TRUE
#   )
#   message("✅ Fiche créée : Fiche-station-", id, ".pdf")
# }

#-------------------------------------------------------------------------------
# Format html
#-------------------------------------------------------------------------------

# for (i in seq_len(nrow(stations_etude))) {
#   id <- stations_etude$sta_code_sandre[i]
#   nom <- gsub("'", "’", stations_etude$sta_libelle_sandre[i])
#   
#   
#   
#   message("Génération de la fiche ",
#           i,
#           "/",
#           nrow(stations_etude),
#           " - Station ",
#           id,
#           " (",
#           nom,
#           ")")
#   
#   rmarkdown::render(
#     here::here("R/fiches_stations", "080_Fiches_stations_html.Rmd"),
#     #Template
#     output_format = "html_document",
#     output_dir = "Output/8_Fiches_stations/Fiches-stations-html",
#     #Fichier de sortie
#     output_file =
#       paste0("Fiche-station-", id, ".html"),
#     params = list(
#       code_station = id,
#       libelle_station = nom,
#       auteur = "OFB DR Normandie"
#     ),
#     clean = TRUE,
#     quiet = TRUE
#   )
#   message("Fiche créée : Fiche-station-", id, ".html")
# }

#-------------------------------------------------------------------------------
# Format word
#-------------------------------------------------------------------------------

for (i in seq_len(nrow(stations_etude))) {
  id <- stations_etude$sta_code_sandre[i]
  nom <- gsub("'", "’", stations_etude$sta_libelle_sandre[i])



  message("Génération de la fiche ",
          i,
          "/",
          nrow(stations_etude),
          " - Station ",
          id,
          " (",
          nom,
          ")")

  rmarkdown::render(
    here::here("R/fiches_stations", "080_Fiches_stations_html.Rmd"),
    #Template
    output_format = "word_document",
    output_dir = "Output/8_Fiches_stations/Fiches-stations-word",
    #Fichier de sortie
    output_file =
      paste0("Fiche-station-", id, ".docx"),
    params = list(
      code_station = id,
      libelle_station = nom,
      auteur = "Service Régional Connaissance & Services Départementaux, OFB, Direction Régionale Normandie"
    ),
    clean = TRUE,
    quiet = TRUE
  )
  message("Fiche créée : Fiche-station-", id, ".docx")
}

# Supprimer les éventuels fichiers temporaires restants
unlink("Script/figures", recursive = TRUE)
unlink("Script/*.log")
unlink("Script/*.aux")
unlink("Script/*.tex")
unlink("Script/*.out")

