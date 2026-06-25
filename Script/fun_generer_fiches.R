library(readr)
library(here)
library(tidyverse)
library(here)

# lecture des stations
stations_etude <- read_delim(
  "Output/stations_etude.csv",
  delim = ";",
  show_col_types = FALSE
) %>% 
  filter(sta_code_sandre %in% c("03250475", "03265993"))

# création du dossier de sortie
dir.create("../Output/Fiches-stations", recursive = TRUE, showWarnings = FALSE)
dir.create("Script/figures", showWarnings = FALSE)
# boucle
for (i in seq_len(nrow(stations_etude))) {
  
  id <- stations_etude$sta_code_sandre[i]
  nom <- gsub("'", "’", stations_etude$sta_libelle_sandre[i])

  
  
  message("Génération de la fiche ", i, "/", nrow(stations_etude),
          " - Station ", id, " (", nom, ")")
  
  rmarkdown::render(
    here::here("Script", "080_Fiches_stations_pdf.Rmd"),
    output_format = "pdf_document",
    output_dir = "../Output/Fiches-stations",
    output_file = 
      paste0("Fiche-station-", id, ".pdf"),
    params = list(
      code_station = id,
      libelle_station = nom,
      auteur = "OFB DR Normandie"
    ),
    clean=TRUE
  )
}
unlink("Script/figures", recursive = TRUE)
unlink("Script/*.log")
unlink("Script/*.aux")
unlink("Script/*.tex")
unlink("Script/*.out")
