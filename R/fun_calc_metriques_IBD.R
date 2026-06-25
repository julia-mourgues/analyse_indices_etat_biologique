# Ce script contient un ensemble de fonctions permettant de mettre à jour les
#listes faunistques et de calculer des métriques de diversité sur les diatomées

# Ce script est une version réadaptée du script 1.3.0 disponible sur le SEEE

# Chargement des packages
dependencies <- c("dplyr", "tidyr")

loadDependencies <- function(dependencies) {
  suppressAll <- function(expr) {
    suppressPackageStartupMessages(suppressWarnings(expr))
  }
  
  lapply(dependencies,
         function(x)
         {
           suppressAll(library(x, character.only = TRUE))
         }
  )
  invisible()
}

loadDependencies(dependencies)

# Chargement des tables

IBD_params <- read.csv2("../R/algo_seee/IBD/1.3.0/IBD_params.csv", stringsAsFactors = FALSE,fileEncoding = "latin1") %>% 
  dplyr::select_at(.vars = c("AFNOR", "SANDRE", paste0("CL", seq(7)), "Val.Ind."))


FunParamsIBD <- function (data, params=IBD_params) {
  
  data <- data %>% 
    left_join(params %>% 
                dplyr::select(SANDRE, AFNOR, Val.Ind.),
              by= c("code_taxon_sandre"="SANDRE"))
}

id_cols <- c(
  "code_station","libelle_station","code_departement","reseaux",
  "code_prelevement","date_prelevement","annee",
  "code_support","libelle_support",
  "code_indice","libelle_indice","Typologie",
  "Typologie_clean","libelle_typologie"
)


## Fonction permettant de calculer les métriques
funMetriquesIBD <- function(Table) {
  
  Table2 <- Table %>%
    group_by(
      code_station,
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
      code_taxon_lettre
    ) %>%
    mutate(resultat_taxon = sum(resultat_taxon))
  
  funMetriques_esp_contributives <- function (df){
    
    df %>%
      group_by(
        across(all_of(id_cols))) %>%
      filter(!is.na(Val.Ind.)) %>%
      summarise(
        `Nombre de taxons contributifs` = n_distinct(code_taxon_lettre),
        `Abondance totale esp contributives`= if (all(type_resultat =="NbrTax")){
          sum(resultat_taxon)
        } else {
          NA_real_
        },
        `Diversite de Shannon esp contributives` = if (all(type_resultat =="NbrTax")) {
          p <- resultat_taxon / sum(resultat_taxon, na.rm = TRUE)
          p <- p[p > 0]
          - sum(p * log(p))
        } else {
          NA_real_
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
      filter(is.na(Val.Ind.)) %>%
      summarise(
        `Nombre de taxons non contributifs` = n_distinct(code_taxon_lettre),
        `Abondance totale esp non contributives`= if (all(type_resultat =="NbrTax")){
          sum(resultat_taxon)
        } else {
          NA_real_
        },
        .groups = "drop")
  }
  
  
  funMetriques_all <- function (df){
    
    df %>%
      group_by(
        across(all_of(id_cols))) %>%
      summarise(
        `Richesse specifique` = n_distinct(code_taxon_lettre),
        `Abondance totale`= if (all(type_resultat =="NbrTax")){
          sum(resultat_taxon)
        } else {
          NA_real_
        },
        `Diversite de Shannon` = if (all(type_resultat =="NbrTax")) {
          p <- resultat_taxon / sum(resultat_taxon, na.rm = TRUE)
          p <- p[p > 0]
          - sum(p * log(p))
        } else {
          NA_real_
        },
        `Equitabilite de Pielou` = ifelse(`Richesse specifique` <= 1,
                                                        0,
                                                        `Diversite de Shannon` /log(`Richesse specifique`)),
        .groups = "drop")
  }
  
  
  # Regrouper les tables
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

#' Title Fonction permettant de calculer les Fi (probabilite de presence des taxons dans les differentes classes)
#'
#' @param table : Tableau d'inventaire (avec les abondances relatives)
#' @param param : Tableau des profils des taxons
#'
#' @return : tableau avec le calcul des Fi
#' @export
#'
#' @examples : funFi(table = resultatsAx, param = IBD_params) 
#' 
funFi <- function(table, param) {
  
  table %>%
    left_join(param,
              by = c("code_taxon_sandre" = "SANDRE",
                     "code_taxon_lettre" = "AFNOR")) %>%
    
    pivot_longer(
      cols = starts_with("CL"),
      names_to = "classe",
      values_to = "Px"
    ) %>%
    
    group_by(
      code_station, libelle_station, code_departement,
      code_prelevement, date_prelevement, annee,
      code_support, libelle_support,
      Typologie,
      Typologie_clean, libelle_typologie,
      classe
    ) %>%
    
    mutate(abondance_totale = sum(resultat_taxon)) %>%
    mutate(abondance_relative = resultat_taxon / abondance_totale) %>%
    
    summarise(
      resultat_metrique =
        sum(abondance_relative * Px * Val.Ind.) /
        sum(abondance_relative * Val.Ind.),
      .groups = "drop"
    ) %>%
    
    mutate(
      code_indice = "IBD",
      libelle_metrique = paste0("Fi_", classe)
    ) %>%
    
    dplyr::select(
      code_station, libelle_station, code_departement,
      code_prelevement, date_prelevement, annee,
      code_support, libelle_support,
      Typologie,
      Typologie_clean, libelle_typologie,
      code_indice, libelle_metrique, resultat_metrique
    )
}
