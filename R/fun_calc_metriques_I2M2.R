# Ce script contient un ensemble de fonctions permettant de mettre à jour les
#listes faunistques et de calculer des métriques de diversité sur les invertébrés

# Ce script est une version réadaptée du script 1.0.6 disponible sur le SEEE

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

#Chargement des tables

table_transcodage_I2M2 <- read.csv2("../R/algo_seee/I2M2/1.0.6/I2M2_params_transcodage.csv", 
                                    header = TRUE, stringsAsFactors = FALSE, 
                                    colClasses = c(CODE_TAXON   = "character",
                                                   CODE_METHODE = "character"))

Base_I2M2  <- read.csv2("../R/algo_seee/I2M2/1.0.6/I2M2_params_base.csv",
                        colClasses = c(cd_taxon = "character"))


## Fonction permettant d'associer les nouveaux codes aux taxons
funTranscodeI2M2<- function(Table, Transcodage=table_transcodage_I2M2) {
  left_join(x = Table, 
            y = Transcodage %>% 
             dplyr::select(CODE_TAXON, CODE_METHODE) %>% 
              mutate(CODE_TAXON= as.numeric(CODE_TAXON)),
            by = c("code_taxon_sandre"="CODE_TAXON")) 
}



id_cols <- c(
  "code_station","libelle_station","code_departement","reseaux",
  "code_prelevement","date_prelevement","annee",
  "code_support","libelle_support",
  "code_indice","libelle_indice",
  "Typologie","Typologie_clean","libelle_typologie"
)

## Fonction permettant de calculer les effectifs par regroupement de bocaux
funEffBocaux <- function(Table) {
  ungroup(Table) %>%
    (function(df) {
      bind_rows(
        filter(df, code_lot %in% c("A", "B", 1, 2))         %>%
          mutate(code_lot = "AB")                           %>%
          group_by(code_station,
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
                   code_taxon_sandre,
                   code_lot, CODE_METHODE)  %>%
          summarise(resultat_taxon   = sum(resultat_taxon)),
        filter(df, code_lot %in% c("B", "C", 2, 3))         %>%
          mutate(code_lot = "BC")                           %>%
          group_by(code_station,
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
                   code_taxon_sandre,
                   code_lot, CODE_METHODE)  %>%
          summarise(resultat_taxon   = sum(resultat_taxon)),
        filter(df, code_lot %in% c("A", "B", "C", 1, 2, 3)) %>%
          mutate(code_lot = "ABC")                          %>%
          group_by(code_station,
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
                   code_taxon_sandre,
                   code_lot, CODE_METHODE)  %>%
          summarise(resultat_taxon   = sum(resultat_taxon))
      )
    })           %>%
    filter(resultat_taxon > 0)
}

funEffBocaux_hubeau <- function(Table) {
  Table %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      phase = toupper(substr(code_lot, 1, 1))
    ) %>%
    (function(df) {
      dplyr::bind_rows(
        # AB
        dplyr::filter(df, phase %in% c("A", "B", "1", "2")) %>%
          dplyr::mutate(code_lot = "AB") %>%
          dplyr::group_by(
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
            code_taxon_sandre,
            code_lot,
            CODE_METHODE
          ) %>%
          dplyr::summarise(
            resultat_taxon = sum(resultat_taxon, na.rm = TRUE),
            .groups = "drop"
          ),
        # BC
        dplyr::filter(df, phase %in% c("B", "C", "2", "3")) %>%
          dplyr::mutate(code_lot = "BC") %>%
          dplyr::group_by(
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
            code_taxon_sandre,
            code_lot,
            CODE_METHODE
          ) %>%
          dplyr::summarise(
            resultat_taxon = sum(resultat_taxon, na.rm = TRUE),
            .groups = "drop"
          ),
        # ABC
        dplyr::filter(df, phase %in% c("A", "B", "C", "1", "2", "3")) %>%
          dplyr::mutate(code_lot = "ABC") %>%
          dplyr::group_by(
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
            code_taxon_sandre,
            code_lot,
            CODE_METHODE
          ) %>%
          dplyr::summarise(
            resultat_taxon = sum(resultat_taxon, na.rm = TRUE),
            .groups = "drop"
          )
      )
    }) %>%
    dplyr::filter(resultat_taxon > 0)
}




## Fonction permettant de calculer les metriques
funMetriquesI2M2 <- function(Table, Base=Base_I2M2) {
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
      code_lot,
      CODE_METHODE,
      code_taxon_sandre
      ) %>%
    summarise(
      resultat_taxon = sum(resultat_taxon, na.rm = TRUE),.groups = "drop")
  
  funMetriques_esp_contributives <- function (df){
    
    df %>%
      group_by(
        across(all_of(id_cols))) %>%
      filter(!is.na(CODE_METHODE)) %>%
      summarise(
        `Nombre de taxons contributifs` = n_distinct(CODE_METHODE),
        `Abondance totale esp contributives`= sum(resultat_taxon),
        `Diversite de Shannon esp contributives` = {
          p <- resultat_taxon / sum(resultat_taxon)
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
      filter(is.na(CODE_METHODE)) %>%
      summarise(
        `Nombre de taxons non contributifs` = n_distinct(code_taxon_sandre),
        `Abondance totale esp non contributives`= sum(resultat_taxon),
        .groups = "drop"
      )
  }
  
  funMetriques_all <- function (df){
    
    df %>%
      group_by(
        across(all_of(id_cols))) %>%
      summarise(
        `Abondance totale`= sum(resultat_taxon),
        `Richesse specifique` = n_distinct(code_taxon_sandre),
        `Diversite de Shannon` = {
          p <- resultat_taxon / sum(resultat_taxon)
          p <- p[p > 0]
          - sum(p * log(p))
        },
        `Equitabilite de Pielou` = ifelse(`Richesse specifique` <= 1,
                                                       0,
                                                       `Diversite de Shannon` /
                                                         log(`Richesse specifique`)),
        .groups = "drop"
      )
  }
  
  
  
  
  ## Fonction permettant de calculer la metrique aspt
  funASPT <- function(df) {
    df %>%
      filter(
        code_lot=="BC",
        resultat_taxon > 0) %>%
      left_join(Base, by = c("CODE_METHODE" = "cd_taxon")) %>%
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
        BMWP.taxo
      ) %>%
      summarise(
        resultat_taxon = sum(resultat_taxon),
        SCORE          = first(BMWP.Original.Score),
        .groups        = "drop"
      ) %>%
      filter(!is.na(SCORE)) %>%
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
        libelle_typologie
      ) %>%
      summarise(
        `ASPT (I2M2)`= mean(SCORE),
        .groups = "drop"
      )
  }
  
  
  ## Fonction permettant de calculer la metrique polyvoltin
  funPolyvoltin <- function(df) {
    df %>%
      filter(
        code_lot=="ABC",
        resultat_taxon > 0)                                   %>%
      left_join(Base, by = c("CODE_METHODE" = "cd_taxon")) %>%
      group_by(across(all_of(id_cols))) %>%
      summarise(`Polyvoltinisme (I2M2)` = 
                  sum(log1p(resultat_taxon) * polyvoltine, 
                                     na.rm = TRUE) /
                  sum(log1p(resultat_taxon), na.rm = TRUE),
                .groups = "drop")
    
  }
  
  ## Fonction permettant de calculer la metrique ovovivipare
  funOvovivipare <- function(df) {
    df %>%
      filter( 
        code_lot=="ABC",
        resultat_taxon > 0)                                   %>%
      left_join(Base, by = c("CODE_METHODE" = "cd_taxon")) %>%
      group_by(
        across(all_of(id_cols))) %>%
      
      summarise(`Ovoviviparite (I2M2)`= sum(log1p(resultat_taxon) * ovoviviparity,
                                    na.rm = TRUE) /
                  sum(log1p(resultat_taxon), na.rm = TRUE),
                .groups = "drop")
  }
  


  ## Calcul des metriques
  funMetriques_esp_contributives(Table2) %>%
    full_join(funMetriques_esp_non_contributives(Table2), by = id_cols) %>%
    full_join(funMetriques_all(Table2), by = id_cols) %>%
    full_join(funASPT(Table2), by = id_cols) %>%
    full_join(funPolyvoltin(Table2), by = id_cols) %>%
    full_join(funOvovivipare(Table2), by = id_cols) %>%
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
        `Equitabilite de Pielou`,
        `ASPT (I2M2)`,
        `Polyvoltinisme (I2M2)`,
        `Ovoviviparite (I2M2)`
      ),
      names_to = "libelle_metrique",
      values_to = "resultat_metrique"
    )
}