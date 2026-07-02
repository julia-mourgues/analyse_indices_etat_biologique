# Type d'algorithme : IBMR
# Auteur(s)         : UDAM (OFB), Irstea
# Date              : 2021-05-27
# Version           : 1.1.5
# Interpreteur	   	: R version 4.0.2 (2020-06-22)
# Pre-requis        : Packages dplyr, tidyr
# Fichiers lies   	: 
# Commentaires 	  	: Indice Biologique Macrophytique en Riviere calcule selon la
# norme NF T90-395 (octobre 2003). Il repose sur 327 taxons pris en compte dans
# le calcul. https://hydrobio-dce.inrae.fr/cours-deau/macrophytes/

# Copyright 2018 UDAM
# Ce programme est un logiciel libre; vous pouvez le redistribuer ou le modifier
# suivant les termes de la GNU General Public License telle que publiee par la
# Free Software Foundation; soit la version 3 de la licence, soit (a votre gre)
# toute version ulterieure.
# Ce programme est distribue dans l'espoir qu'il sera utile, mais SANS AUCUNE
# GARANTIE; sans meme la garantie tacite de QUALITE MARCHANDE ou d'ADEQUATION A
# UN BUT PARTICULIER. Consultez la GNU General Public License pour plus de
# details.
# Vous devez avoir recu une copie de la GNU General Public License en meme temps
# que ce programme; si ce n'est pas le cas, consultez
# <http://www.gnu.org/licenses>.

## VERSION ----
indic  <- "IBMR"
vIndic <- "v1.2.0"

## CHARGEMENT DES PACKAGES ----
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

## IMPORT DES FICHIERS DE CONFIGURATION ----
param_ibmr <- read.csv2("IBMR_params_2023-07-12.csv",
                        colClasses = c(Cd_taxon = "character",
                                       CSi      = "integer",
                                       Ei       = "integer"))
param_transcode <- read.csv2("IBMR_params_transcode_2023-07-12.csv")

## DECLARATION DES FONCTIONS ----
## Fonction permettant de calculer les classes d'abondance
funKi <- function(tableFloristique) {
  group_by(.data = tableFloristique,
           CODE_OPERATION, CODE_TAXON) %>% 
    summarise(RECOUVREMENT = sum(RESULTAT * POURCENTAGE_FACIES) / 100) %>% 
    mutate(Ki = case_when(RECOUVREMENT <  0.1                       ~ 1,
                          RECOUVREMENT >= 0.1 & RECOUVREMENT < 1    ~ 2,
                          RECOUVREMENT >= 1   & RECOUVREMENT < 10   ~ 3,
                          RECOUVREMENT >= 10  & RECOUVREMENT <50    ~ 4,
                          RECOUVREMENT >= 50  & RECOUVREMENT <= 100 ~ 5,
                          TRUE ~ NA_real_))
}

# Fonction permettant de faire le transcodage des taxons
# idem a fonction IBML
funTranscode <- function(data_flore, transcodage) {
# browser()
  dataTranscode <- left_join(data_flore, transcodage, 
            by = c("CODE_TAXON" = "CD_TAXON"))                       %>%
    group_by(CODE_OPERATION, UR, DATE, CODE_FINAL, POURCENTAGE_FACIES)  %>%
    # # on ne somme pas les pourcentages car cela ne voudrait pas dire grand chose (à confirmer !!)
    # en fait on le fait plus tard ! dans le calcul de l'indicateur 
    # summarise(RESULTAT = ifelse(length(RESULTAT) > 1,
    #                             NA,
    #                             unique(RESULTAT, na.rm = TRUE))) %>%
    ungroup() %>%
    select(-c(CODE_TAXON, DATE.MAJ.1)) %>%
    rename("CODE_TAXON" = "CODE_FINAL")
  return(dataTranscode)
}


funIBMR <- function(tableFloristique, param_ibmr) {
  # browser()
  temp <- left_join(x = tableFloristique,
                    y = param_ibmr,
                    by = c("CODE_TAXON" = "Cd_taxon"))
  
  taxa_excluded <- group_by(temp, CODE_OPERATION) %>% 
    filter(Ei * Ki == max(Ei * Ki))               %>% 
    slice(1)                                      %>% 
    mutate(ROBUSTE = FALSE)
  
  # Calcul IBMR
  temp %>% 
    left_join(x = .,
              y = select(taxa_excluded, 
                         CODE_OPERATION, CODE_TAXON, ROBUSTE),
              by = c("CODE_OPERATION", "CODE_TAXON")) %>% 
    mutate(ROBUSTE = if_else(is.na(ROBUSTE), TRUE, ROBUSTE)) %>% 
    group_by(CODE_OPERATION)                   %>% 
    summarise(IBMR = sum(Ei * Ki * CSi) / sum(Ei * Ki),
              IBMR_robuste = 
                if_else(n_distinct(CODE_TAXON) >= 2,
                        sum(Ei[ROBUSTE] * Ki[ROBUSTE] * CSi[ROBUSTE]) /
                          sum(Ei[ROBUSTE] * Ki[ROBUSTE]),
                        NA_real_),
              COMMENTAIRES =
                if_else(n_distinct(CODE_TAXON) >= 2,
                        paste0("Le taxon '", CODE_TAXON[! ROBUSTE],
                               "' a ete supprime pour le calcul de la robustesse."),
                        "Un seul taxon retenu pour le calcul de l'IBMR. La robustesse ne peut etre calculee")) %>% 
    gather(key = LIB_PAR, value = RESULTAT, -CODE_OPERATION, -COMMENTAIRES) %>% 
    mutate(COMMENTAIRES = if_else(LIB_PAR == "IBMR_robuste",
                                  COMMENTAIRES, NA_character_),
           CODE_PAR = case_when(LIB_PAR == "IBMR"         ~ "2928",
                                LIB_PAR == "IBMR_robuste" ~ "8063")) %>% 
    select(CODE_OPERATION, CODE_PAR, LIB_PAR, RESULTAT, COMMENTAIRES)
}

# Fonction permettant le calcul des taxons contributifs
funContributifs <- function(data_entree, param_ibmr) {
  group_by(data_entree, CODE_OPERATION,CODE_SANDRE_TAXON, CODE_TAXON) %>% 
    summarise(RESULTAT = sum(RESULTAT)) %>% 
    left_join(x = .,
              y = param_ibmr,
              by = c("CODE_TAXON" = "Cd_taxon")) %>% 
    group_by(CODE_OPERATION) %>%
    summarise(CODE_PAR = "7974",
              LIB_PAR = "NbTaxonsIBMRcontributifs",
              RESULTAT = n_distinct(CODE_SANDRE_TAXON[! is.na(CSi)]),
              COMMENTAIRES = paste0(
                "Les taxons suivant, representant ",
                round(100 * n_distinct(CODE_TAXON[is.na(CSi)]) / n_distinct(CODE_TAXON)),
                "% des taxons, n'ont pas ete pris en compte dans le calcul: ",
                paste(CODE_TAXON[is.na(CSi)], collapse = ", "),
                ". ",
                if (any(is.na(CODE_TAXON))) { 
                  paste0("Le code 6L des taxons suivants : ", 
                         paste(CODE_SANDRE_TAXON[is.na(CODE_TAXON)], collapse = ", "), 
                         " n'est pas reconnu.") 
                } else {""}))
}

# Fonction permettant le calcul des résultats complémentaires
funComplementaire <- function(data_entree, param_ibmr) {
  group_by(data_entree, CODE_OPERATION) %>% 
    summarise(LIB_PAR = "NbTaxons",
              RESULTAT = n_distinct(CODE_TAXON)) %>% 
    bind_rows(filter(data_entree, CODE_TAXON %in% param_ibmr$Cd_taxon) %>% 
                select(CODE_OPERATION, CODE_TAXON) %>% 
                distinct() %>% 
                left_join(x = .,
                          y = param_ibmr,
                          by = c("CODE_TAXON" = "Cd_taxon")) %>% 
                group_by(CODE_OPERATION) %>% 
                summarise(CSi_min = min(CSi),
                          CSi_moy = mean(CSi),
                          CSi_sd  = sd(CSi),
                          CSi_max = max(CSi),
                          Ei_min  = min(Ei),
                          Ei_moy  = mean(Ei),
                          Ei_sd   = sd(Ei),
                          Ei_max  = max(Ei)) %>% 
                gather(key = LIB_PAR, value = RESULTAT,
                       -CODE_OPERATION)) %>% 
    arrange(CODE_OPERATION)
}

## Fonction permettant de faire les arrondis a l'inferieur si 0 a 4 et au superieur si 5 a 9
funArrondi <- function (x, digits = 0) {
  .local <- function(x, digits) {
    x <- x * (10^digits)
    ifelse(abs(x%%1 - 0.5) < .Machine$double.eps^0.5,
           ceiling(x)/(10^digits),
           round(x)/(10^digits)) %>% 
      trimws() # trims permet d'enlever l'espace exedentaire
  }

  if (is.data.frame(x))
    return(data.frame(lapply(x, .local, digits)))
  .local(x, digits)
}

## Fonction initialisant le fichier de sortie
funSortie <- function(data_entree, paramsOut, ...) {
  select(data_entree, ...) %>%
    distinct()             %>%
    (function(df) {
      df[rep(1:nrow(df), each = nrow(paramsOut)),] %>%
        as.tbl()
    })                     %>%
    mutate(CODE_PAR = rep(paramsOut$CODE_PAR,
                          n() / nrow(paramsOut)),
           LIB_PAR  = rep(paramsOut$LIB_PAR,
                          n() / nrow(paramsOut)))
}

## Fonction permettant d'ecrire le fichier de sortie
funResult 		<- function(indic, vIndic, heure_debut,
                        data_sortie, data_complementaire, complementaire,
                        file, file_complementaire)
{
    # determination du temps de calcul
    heure_fin       <- Sys.time()
    heure_dif       <- heure_fin - heure_debut
    temps_execution <- paste0(round(heure_dif, 2),
                              attr(heure_dif, "units"))

    # creation du bandeau d'information
    etiquette <- paste(indic, vIndic, Sys.Date(),
                       "Temps d'execution :", temps_execution,
                       sep = ";")

    # sortie du bandeau d'information
    cat(paste0(etiquette, "\n"), file = file, sep = "")

    # sortie du fichier de sortie
    write.table(data_sortie, row.names = FALSE, quote = FALSE, sep = ";",
                file = file, append = TRUE)

    # Sortie complementaire
    if(complementaire)
    {
        if (file == "") {
            print("Fichier")
        }

        cat(paste0(etiquette, "\n"), file = file_complementaire, sep = "")
        write.table(data_complementaire, row.names = FALSE, quote = FALSE,
                    sep = ";", file = file_complementaire, append = TRUE)
    }

}# fin de la fonction funResult

## INITIALISATION DU TRAITEMENT ----
# Ne pas afficher les messages d'avis ni d'erreur
options(warn = -1)

File           <- "./../../Documentation/datasets/FLORE_MACROPHYTES_FORMAT_NAIADES_supprpuissance.txt"
complementaire <- TRUE

# Initialisation de l'heure
heure_debut <- Sys.time()

##  IMPORT DES FICHIERS ----
# Import du fichier d'entree
data_entree <- read.table(File, header = TRUE, sep = "\t",
                          stringsAsFactors = FALSE, quote = "\"", 
                          colClasse = c(CODE_OPERATION     = "character",
                                        CODE_STATION       = "character",
                                        DATE               = "character",
                                        CODE_TAXON         = "character",
                                        UR                 = "character",
                                        POURCENTAGE_FACIES = "numeric",
                                        RESULTAT           = "numeric"))

## INITIALISATION DU FICHIER DE SORTIE ----
paramsOut <- data.frame(CODE_PAR = c("7974", "2928", "8063"),
                        LIB_PAR  = c("NbTaxonsIBMRcontributifs",
                                     "IBMR","IBMR_robuste"),
                        stringsAsFactors = FALSE)

data_sortie <- funSortie(data_entree = data_entree,
                         paramsOut   = paramsOut,
                         CODE_OPERATION, CODE_STATION, DATE) %>%
  mutate(CODE_OPERATION = as.character(CODE_OPERATION),
         CODE_STATION   = as.character(CODE_STATION),
         DATE           = as.character(DATE))

## CALCUL DE L'INDICE ----
data_entree_transcode <- 
  data_entree %>% 
  # transcodes des données flores
  funTranscode(data_flore = ., transcodage = param_transcode)

resultats <- data_entree_transcode %>%
  # Selection des taxons retenus pour le calcul de l'IBMR
  filter(CODE_TAXON %in% param_ibmr$Cd_taxon) %>% 
  # Regroupement des taxons identiques par operation avec somme des effectifs
  group_by(CODE_OPERATION, CODE_TAXON, UR) %>% 
  summarise(RESULTAT = sum(RESULTAT), 
            POURCENTAGE_FACIES = unique(POURCENTAGE_FACIES)) %>% 
  # Calcul des coefficients d'abondance
  funKi(tableFloristique = .) %>% 
  # Calcul de l'IBMR
  funIBMR(tableFloristique = ., param_ibmr = param_ibmr) %>% 
  # Taxons contributifs
  bind_rows(.,
            funContributifs(data_entree_transcode, param_ibmr)) %>% 
  arrange(CODE_OPERATION)

## RESULTATS COMPLEMENTAIRES ----
if (complementaire) {
  data_complementaire <- funComplementaire(data_entree, param_ibmr) %>% 
    mutate(RESULTAT = if_else(grepl(x = LIB_PAR, pattern = "_moy$|_sd$"),
                              format(funArrondi(RESULTAT, 2), nsmall = 2, trim = TRUE),
                              format(RESULTAT, digits = 2, nsmall = 0, trim = TRUE))) %>% 
    left_join(x = select(data_sortie, CODE_OPERATION, CODE_STATION, DATE) %>% 
                distinct(),
              y = .,
              by = "CODE_OPERATION")
} else {
  data_complementaire <- NULL
}

## SORTIE DES RESULTATS ----

data_sortie <- left_join(x  = data_sortie,
                         y  = resultats,
                         by = c("CODE_OPERATION", "CODE_PAR", "LIB_PAR")) %>% 
  mutate(RESULTAT = if_else(CODE_PAR %in% c("2928", "8063"),
                            format(funArrondi(RESULTAT, 2), nsmall = 2),
                            format(RESULTAT, digits = 2, nsmall = 0)),
         COMMENTAIRES = if_else(is.na(COMMENTAIRES), "", COMMENTAIRES))


fichierResultat               <- paste0(indic, "_", vIndic, "_resultats.csv")
fichierResultatComplementaire <- paste0(indic, "_", vIndic,
                                        "_resultats_complementaires.csv")
funResult(indic               = indic,
          vIndic              = vIndic,
          heure_debut         = heure_debut,
          data_sortie         = data_sortie,
          data_complementaire = data_complementaire,
          complementaire      = complementaire,
          file                = fichierResultat,
          file_complementaire = fichierResultatComplementaire)

