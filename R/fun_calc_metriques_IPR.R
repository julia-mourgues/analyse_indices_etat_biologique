# Ce script contient un ensemble de fonctions permettant de mettre à jour les
#listes faunistiques et de calculer des métriques de diversité sur les poissons


# Objet qui va permettre de grouper les données
id_cols1 <- c(
  "code_station",
  "code_sta_pp",
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
  "libelle_taxon",
  "ope_surface_calculee_peche",
  "type_peche",
  "numero_passage_peche",
  "type_resultat",
  "operateur"
)


## Fonction permettant de regrouper certains taxons
## Pour les opération de pêche par Passage, on s'intéresse seulement au passage n°1.

funRegroup <- function(Table) {
  
  # 1. Agrégation 
  Table2 <- Table %>%
    group_by(across(all_of(id_cols1))) %>%
    mutate(code_taxon_lettre = as.character(code_taxon_lettre)) %>%
    filter(
      type_peche != "Passage" | 
        (type_peche == "Passage" & numero_passage_peche == 1)
    ) %>% 
    summarise(resultat_taxon = sum(resultat_taxon),
              .groups = "drop")
  
  # 2. Correction des codes X et ?

  Table_corrige <- Table2 %>%
    group_by(code_prelevement) %>%
    mutate(all_codes = list(code_taxon_lettre)) %>%
    rowwise() %>%
    mutate (
      pref = str_sub(code_taxon_lettre, 1, 2),
      is_unknown = str_ends(code_taxon_lettre, "X|\\?"),
      
      matches = list(if (is_unknown) {
        all_codes[!str_ends(all_codes, "X|\\?") &
                    str_starts(all_codes, pref)]
      } else {
        character(0)
      }),
      
      nb_match = if (is_unknown)
        length(matches)
      else
        NA_integer_,
      
      code_corrige = case_when(
        # présence d'un "?"
        
        str_ends(code_taxon_lettre, "\\?") &
          nb_match == 0 ~ NA_character_,
        
        str_ends(code_taxon_lettre, "\\?") & nb_match == 1 ~
          if (length(matches) > 0)
            matches[[1]][1]
        else
          NA_character_,
        str_ends(code_taxon_lettre, "\\?") &
          nb_match > 1 & pref == "LP" ~ "LPP",
        
        # Présence d'un "X"
        str_ends(code_taxon_lettre, "X") & nb_match == 1 ~
          if (length(matches) > 0)
            matches[[1]][1]
        else
          NA_character_ ,
        
        # Si code espèce ="GOX"
        code_taxon_lettre == "GOX" ~ "GOU",
        
        # Si code espèce = "LPX"
        code_taxon_lettre == "LPX" & nb_match == 0 ~ "LPP",
        code_taxon_lettre == "LPX" & nb_match > 1 ~ "LPP",
        
        # Si code espèce = "BRX"
        code_taxon_lettre == "BRX" & nb_match == 0 ~ "BRE",
        code_taxon_lettre == "BRX" & nb_match > 1 ~ NA_character_,
        
        # autres cas
        TRUE ~ code_taxon_lettre
      )
    ) %>%
    ungroup() %>%
    filter(!is.na(code_corrige)) %>%
    select(-all_codes, -matches, -pref, -is_unknown)
  
  # 3. Re-agrégation après correction
  Table_final <- Table_corrige %>%
    group_by(across(all_of(id_cols1))) %>%
    mutate(code_taxon_lettre = code_corrige) %>%
    summarise(resultat_taxon = sum(resultat_taxon),
              .groups = "drop")
  
  return(Table_final)
}

funMetriquesIPR <- function(data_entree) {
  # Colonnes d'identification d'un prélèvement
  id_cols <- c(
    "code_station",
    "code_sta_pp",
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
    ungroup()
  
  
  # Fonction générique pour calculer les métriques
  calc_metriques <- function(df, prefix) {
    df %>%
      group_by(across(all_of(id_cols))) %>%
      summarise(
        richesse = n_distinct(code_taxon_lettre),
        abondance_totale = sum(resultat_taxon, na.rm = TRUE),
        
        shannon = {
          p <- resultat_taxon / sum(resultat_taxon, na.rm = TRUE)
          p <- p[p > 0]
          - sum(p * log(p))
        },
        
        pielou = ifelse(richesse <= 1, 0, shannon / log(richesse)),
        
        .groups = "drop"
      ) %>%
      rename(
        !!paste0("Richesse specifique", prefix) := richesse,!!paste0("Abondance totale", prefix) := abondance_totale,!!paste0("Diversite de Shannon", prefix) := shannon,!!paste0("Equitabilite de Pielou", prefix) := pielou
      )
  }
  met_all <- data %>%
    calc_metriques("")
  
  result<-met_all %>%
    pivot_longer(
      cols = -all_of(id_cols),
      names_to = "libelle_metrique",
      values_to = "resultat_metrique"
    ) %>% 
    mutate(resultat_metrique= round(resultat_metrique,2))
  
  return(result)
}
