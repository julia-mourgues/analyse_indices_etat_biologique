# Ce script contient un ensemble de fonctions permettant de mettre à jour les
#listes faunistques et de calculer des métriques de diversité sur les poissons

# La fonction 'funRegroup' est une version réadaptée de celle du script 1.0.3 disponible sur le SEEE


## IMPORT DES FICHIERS DE CONFIGURATION ----
especes_contrib	<- read.csv2("../R/algo_seee/IPR/1.0.3/IPR_params_Especes_Contributives.csv")
especes_regroup <- as.character(especes_contrib[especes_contrib$REGROUP != "", "Taxon"])

## DECLARATION DES FONCTIONS ----
id_cols1 <- c(
  "code_station",
  "libelle_station",
  "code_departement",
  "reseaux",
  "code_prelevement",
  "date_prelevement",
  "annee",
  "code_support",
  "libelle_support",
  "code_indice",
  "libelle_indice",
  "Typologie",
  "Typologie_clean",
  "libelle_typologie",
  "code_taxon_lettre",
  "ope_surface_calculee_peche",
  "type_peche",
  "numero_passage_peche"
)

especes_contrib$Taxon   <- trimws(as.character(especes_contrib$Taxon))
especes_contrib$REGROUP <- trimws(as.character(especes_contrib$REGROUP))

code_taxon_contributifs <- especes_contrib %>%
  mutate(code = case_when(NTE == "R" & REGROUP != "" ~ REGROUP, TRUE ~ Taxon)) %>%
  pull(code)

code_taxon_contributifs <- unique(code_taxon_contributifs)

## Fonction permettant de regrouper certains taxons
funRegroup <- function(Table) {
  # Nettoyage des codes
  Table$code_taxon_lettre <- trimws(as.character(Table$code_taxon_lettre))
  
  
  # Jointure pour récupérer REGROUP
  Table <- Table %>%
    left_join(
      especes_contrib %>% dplyr::select(Taxon, REGROUP),
      by = c("code_taxon_lettre" = "Taxon")
    ) %>%
    
    # Remplacement vectorisé
    mutate(code_taxon_lettre = case_when(
      REGROUP != "" & !is.na(REGROUP) ~ REGROUP,
      TRUE ~ code_taxon_lettre
    )) %>%
    
    # On enlève la colonne REGROUP temporaire
    dplyr::select(-REGROUP)
  
  # Fusion des lignes identiques
  Table <- Table %>%
    group_by(across(all_of(id_cols1))) %>%
    summarise(resultat_taxon = sum(resultat_taxon),
              .groups = "drop")
  
  return(Table)
}


funMetriquesIPR <- function(data_entree) {
  # Colonnes d'identification d'un prélèvement
  id_cols <- c(
    "code_station",
    "libelle_station",
    "code_departement",
    "reseaux",
    "code_prelevement",
    "date_prelevement",
    "annee",
    "code_support",
    "libelle_support",
    "code_indice",
    "libelle_indice",
    "Typologie",
    "Typologie_clean",
    "libelle_typologie"
  )
  
  # Préparation des données : densité + agrégation par taxon
  data <- data_entree %>%
    ungroup() %>%
    mutate(
      densite_taxon = ceiling(resultat_taxon / ope_surface_calculee_peche),
      code_taxon_lettre = as.character(code_taxon_lettre)
    ) %>%
    filter(
      type_peche != "Passage" | 
        (type_peche == "Passage" & numero_passage_peche == 1)
    )
  
  
  
  # Fonction générique pour calculer les métriques
  calc_metriques <- function(df, prefix) {
    df %>%
      group_by(across(all_of(id_cols))) %>%
      summarise(
        richesse = n_distinct(code_taxon_lettre),
        densite_totale = sum(densite_taxon, na.rm = TRUE),
        
        shannon = {
          p <- resultat_taxon / sum(resultat_taxon, na.rm = TRUE)
          p <- p[p > 0]
          - sum(p * log(p))
        },
        
        pielou = ifelse(richesse <= 1, 0, shannon / log(richesse)),
        
        .groups = "drop"
      ) %>%
      rename(
        !!paste0("Richesse specifique", prefix) := richesse,!!paste0("Abondance totale", prefix) := densite_totale,!!paste0("Diversite de Shannon", prefix) := shannon,!!paste0("Equitabilite de Pielou", prefix) := pielou
      )
  }
  
  # 1. Espèces contributives
  met_contrib <- data %>%
    filter(code_taxon_lettre %in% code_taxon_contributifs) %>%
    calc_metriques(" esp contributives")
  
  # 2. Espèces non contributives
  met_non_contrib <- data %>%
    filter(!(code_taxon_lettre %in% code_taxon_contributifs)) %>%
    calc_metriques(" esp non contributives") %>%
    dplyr::select(-contains("Diversite"), -contains("Equitabilite"))  # pas de Shannon/Pielou ici
  
  # 3. Toutes espèces
  met_all <- data %>%
    calc_metriques("")
  
  # Fusion + pivot long
  met_contrib %>%
    full_join(met_non_contrib, by = id_cols) %>%
    full_join(met_all, by = id_cols) %>%
    pivot_longer(
      cols = -all_of(id_cols),
      names_to = "libelle_metrique",
      values_to = "resultat_metrique"
    )
  
}
