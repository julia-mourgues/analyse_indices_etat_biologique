# Ce script contient un ensemble de fonctions permettant de mettre à jour les
#listes floristiques et de calculer des métriques de diversité sur les macrophytes

# Ce script est une version réadaptée du script 1.2.1 disponible sur le SEEE

# Chargement des packages
dependencies <- c("dplyr", "tidyr")

loadDependencies <- function(dependencies) {
  suppressAll <- function(expr) {
    suppressPackageStartupMessages(suppressWarnings(expr))
  }
  
  lapply(dependencies, function(x)
  {
    suppressAll(library(x, character.only = TRUE))
  })
  invisible()
}

loadDependencies(dependencies)

# Chargement des tables
table_ibmr <- read.csv2(here::here("R/algo_seee/IBMR/1.2.1","IBMR_params.csv" ),
  colClasses = c(Cd_taxon = "character", 
                 CSi      = "integer", 
                 Ei       = "integer")
)
table_transcodage_IBMR <- read.csv2(here::here("R/algo_seee/IBMR/1.2.1","IBMR_params_transcode.csv" ))


# Fonction permettant de faire le transcodage des taxons contributifs de l'IBMR

funTranscodeIBMR <- function(data, Transcodage = table_transcodage_IBMR) {
  dataTranscode <- left_join(data, 
                             Transcodage, 
                             by = c("code_taxon_sandre" = "CD_SANDRE")) %>%
    ungroup() %>%
   dplyr::select(-DATE.MAJ.1, -CD_TAXON, -NOM_TAXON) %>%
    rename(code_taxon_lettre = CODE_FINAL)
  return(dataTranscode)
}


# Fonction permettant d'associer les scores trophiques et les coefficients
#de stenoecie aux taxons
funIBMR <- function(data, params = table_ibmr) {
  temp <- left_join(
    x = data,
    y = params,
    by = c("code_taxon_lettre" = "Cd_taxon")
  )
}


# Fonction permettant de calculer les classes d'abondance pour chaque taxon (ki)
funKi <- function(tableFloristique) {
  group_by(.data = tableFloristique, code_prelevement, code_taxon_lettre) %>%
    mutate(
      Ki = case_when(
        type_resultat == "RecTax" &
          resultat_taxon <  0.1                       ~ 1,
        type_resultat == "RecTax" &
          resultat_taxon >= 0.1 & resultat_taxon < 1    ~ 2,
        type_resultat == "RecTax" &
          resultat_taxon >= 1   & resultat_taxon < 10   ~ 3,
        type_resultat == "RecTax" &
          resultat_taxon >= 10  & resultat_taxon < 50    ~ 4,
        type_resultat == "RecTax" &
          resultat_taxon >= 50  & resultat_taxon <= 100 ~ 5,
        TRUE ~ NA_real_
      )
    )
}

funRegroupeIBMR <- function(data) {
  data %>%
    group_by(
      code_station,
      code_sta_pp,
      libelle_station,
      code_departement,
      reseaux,
      code_prelevement,
      date_prelevement,
      annee,
      code_support,
      libelle_support,
      code_indice,
      libelle_indice,
      Typologie,
      Typologie_clean,
      libelle_typologie,
      code_taxon_lettre,
      type_resultat
    ) %>%
    summarise(
      resultat_taxon = sum(resultat_taxon, na.rm = TRUE),
      CSi = first(CSi),
      Ei  = first(Ei),
      Ki = first(Ki),
      .groups = "drop"
    )
}

id_cols <- c(
  "code_station","code_sta_pp","libelle_station","code_departement","reseaux",
  "code_prelevement","date_prelevement","annee",
  "code_support","libelle_support",
  "code_indice","libelle_indice", "Typologie",
  "Typologie_clean","libelle_typologie"
)

## Fonction permettant de calculer les metriques
funMetriquesIBMR <- function(Table) {

  funMetriques_esp_contributives <- function (df){

    df %>%
      group_by(
        across(all_of(id_cols))) %>%
      filter(!is.na(CSi),
             type_resultat =="RecTax") %>%
      summarise(
        `Nombre de taxons contributifs` = n_distinct(code_taxon_lettre),
        `Abondance totale esp contributives`= sum(resultat_taxon, na.rm = TRUE),
        `Diversite de Shannon esp contributives` = {
          p <- resultat_taxon / sum(resultat_taxon, na.rm = TRUE)
          p <- p[p > 0]
          - sum(p * log(p))
        },
        `Equitabilite de Pielou esp contributives` = ifelse(`Nombre de taxons contributifs` <= 1,
                                                                   0,
                                                                   `Diversite de Shannon esp contributives` /log(`Nombre de taxons contributifs`)),
        .groups = "drop")
  }
  
  funMetriques_esp_non_contributives <- function (df){
    
    df %>%
      group_by(
        across(all_of(id_cols))) %>%
      filter(is.na(CSi),
             type_resultat =="RecTax") %>%
      summarise(
        `Nombre de taxons non contributifs` = n_distinct(code_taxon_lettre),
        `Abondance totale esp non contributives`= sum(resultat_taxon, na.rm = TRUE),
        .groups = "drop")
  }
  
  
  funMetriques_all <- function (df){
    
    df %>%
      group_by(
        across(all_of(id_cols))) %>%
      filter(type_resultat =="RecTax") %>% 
      summarise(
        `Richesse specifique` = n_distinct(code_taxon_lettre),
        `Abondance totale`= sum(resultat_taxon,  na.rm = TRUE),
        `Diversite de Shannon` = {
          p <- resultat_taxon / sum(resultat_taxon, na.rm = TRUE)
          p <- p[p > 0]
          - sum(p * log(p))
        } ,
        `Equitabilite de Pielou` = ifelse(`Richesse specifique` <= 1,
                                                                   0,
                                          `Diversite de Shannon` /log(`Richesse specifique`)),
        .groups = "drop")
  }
  
  
  ## Regrouper les tables
  funMetriques_esp_contributives(Table) %>%
    full_join(funMetriques_esp_non_contributives(Table), by = id_cols) %>%
    full_join(funMetriques_all(Table), by = id_cols) %>%
    tidyr::pivot_longer(
      cols = c(
        `Nombre de taxons contributifs`,
        `Abondance totale esp contributives`,
        `Diversite de Shannon esp contributives`,
        `Equitabilite de Pielou esp contributives`,
        `Nombre de taxons non contributifs`,
        `Abondance totale esp non contributives`,
        `Abondance totale`,
        `Richesse specifique`,
        `Diversite de Shannon`,
        `Equitabilite de Pielou`
      ),
      names_to = "libelle_metrique",
      values_to = "resultat_metrique"
    )
}
# Fonction permettant de calculer les métriques complémentaires
funComplementaireIBMR <- function(data_entree) {
  data_entree %>%
    group_by(
      code_station,
      code_sta_pp,
      libelle_station,
      code_departement,
      reseaux,
      code_prelevement,
      date_prelevement,
      annee,
      code_support,
      libelle_support,
      code_indice,
      libelle_indice,
      Typologie,
      Typologie_clean,
      libelle_typologie
    ) %>%
    summarise(
      `Score trophique moyen`  = mean(CSi, na.rm = TRUE),
      `Coefficient trophique moyen`   = mean(Ei, na.rm = TRUE),
      `Recouvrement moyen`= mean(Ki, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = c(`Score trophique moyen`, `Coefficient trophique moyen`,`Recouvrement moyen`),
      names_to = "libelle_metrique",
      values_to = "resultat_metrique"
    ) %>%
   dplyr::select(
      code_station,
      code_sta_pp,
      libelle_station,
      code_departement,
      reseaux,
      code_prelevement,
      date_prelevement,
      annee,
      code_support,
      libelle_support,
      code_indice,
      libelle_indice,
      Typologie,
      Typologie_clean,
      libelle_typologie,
      libelle_metrique,
      resultat_metrique
    )
}

