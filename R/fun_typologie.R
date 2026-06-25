#' Associer la typologie des stations à leur signification et leur limites inférieures
#'
#' @param code_brut variable contenant le code typologie brut

# 1. Dictionnaires de correspondance 

# Signification des lettres 
dico_lettres <- c(
  TP = "Très petit",
  P = "Petit",
  M = "Moyen",
  G = "Grand",
  TG = "Très grand"
)

# Signification des nombres (hydroécorégions)
dico_nombres <- c(
  "12"  = "Armoricain",
  "9"  = "Tables calcaires"
)


# 2. Fonction de traduction du code 

nettoyage_code <- function (code_brut) {
  code_clean <- sub("-.*$", "", code_brut)
}


traduire_code <- function(code_clean) {
  
  # Extraire lettres et chiffres
  lettres <- sub("([A-Z]+).*", "\\1", code_clean)
  chiffres <- sub(".*?([0-9]+)$", "\\1", code_clean)
  
  # Traductions
  traduction_lettre <- dico_lettres[lettres]
  traduction_nombre <- dico_nombres[chiffres]
  
  # Résultat final
  signification <- paste(traduction_nombre)
  
}
