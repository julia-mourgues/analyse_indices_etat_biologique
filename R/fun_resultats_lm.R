#' Appliquer un modèle linéaire à une liste de d'indice et récupérer les valeurs des modèles
#'
#' @param data dataframe contenant les résultats
#' @param liste_codes_indices liste contenant les indices pour lesquels appliquer le modèle
#' @param col_resultat_indice variable contenant les résultats à représenter
#' @param var_temporelle  variable temporelle du modèle (ex:année)

resultats_lm <- function(data,
                         liste_codes_indices,
                         col_resultat_indice,
                         var_temporelle) {
  modeles <- list()
  resume <- list()
  
  for (i in liste_codes_indices) {
    data_filtre <- data %>%
      dplyr::ungroup() %>%
      dplyr::filter(code_indice == i)
    
    
    # Construire la formule dynamiquement
    formule <- as.formula(paste(col_resultat_indice, "~", var_temporelle))
    
    modele <- lm(formule, data = data_filtre)
    s <- summary(modele)
    
    modeles[[as.character(i)]] <- modele
    
    resume[[as.character(i)]] <- data.frame(
      indice = i,
      intercept = coef(modele)[["(Intercept)"]],
      pente = coef(modele)[[var_temporelle]],
      pvalue = s$coefficients[var_temporelle, "Pr(>|t|)"],
      direction = dplyr::case_when(
        coef(modele)[[var_temporelle]] > 0 ~ "croissante",
        coef(modele)[[var_temporelle]] < 0 ~ "décroissante",
        TRUE ~ "stable"
      ),
      r2 = s$r.squared
    )
  }
  
  list(tableau = dplyr::bind_rows(resume), modeles = modeles)
}
