# Type d'algorithme : IPR
# Auteur(s)         : Cedric MONDY, Delphine CORNEIL
# Date              : 2018-10-05
# Version           : 1.0.3
# Interpreteur      : R version 3.5.1 (2018-07-02)
# Pre-requis        : Packages dplyr
# Fichiers lies     : IPR_params_Coefficientsa.csv, IPR_params_Coefficientsb.csv, IPR_params_Especes_Contributives.csv
# Commentaires      : Indice Poisson Riviere calcule selon la norme NF T90-344
# (juillet 2011). Certains taxons ont ete regroupes et sont donc pris en compte
# dans le calcul de l'IPR : BRB vers BRE; CAS, CAG, CAD, CAA vers CAX; CCU vers
# CCO; CHP vers CHA; CMI vers CCO; GOL, GOO, GOU vers GOX; TRL, TRM vers TRF;
# VAI, VAC, VAB vers PHX; VAR vers VAN. Certaines valeurs de la norme ont fait
# l'objet d'arrondi different. Il s'agit des valeurs utilisees pour le calcul de
# la variable G, des ecarts-type y.

# Copyright 2018 Cedric MONDY
# Ce programme est un logiciel libre; vous pouvez le redistribuer ou le modifier
# suivant les termes de la GNU General Public License telle que publiee par la
# Free Software Foundation; soit la version?3 de la licence, soit (a votre gre)
# toute version ulterieure.
# Ce programme est distribue dans l'espoir qu'il sera utile, mais SANS AUCUNE
# GARANTIE; sans meme la garantie tacite de QUALITE MARCHANDE ou d'ADEQUATION A
# UN BUT PARTICULIER. Consultez la GNU General Public License pour plus de
# details.
# Vous devez avoir recu une copie de la GNU General Public License en meme temps
# que ce programme; si ce n'est pas le cas, consultez
# <http://www.gnu.org/licenses>.

## VERSION ----
indic  <- "IPR"
vIndic <- "v1.0.3"

## CHARGEMENT DES PACKAGES ----
dependencies <- c("dplyr")

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

## DECLARATION DES FONCTIONS ----

## Fonction alternative a ifelse, plus simple
siNon <- function(test, yes, no) {
    if (test) {
        return(yes)
    } else {
        return(no)
    }
}

# Fonction permettant d'initialiser la sortie de tests
initResult <- function() {
    list(verif  = "ok",
         sortie = tibble(colonne = "", ligne = "", message = "")[0,])
}

# Fonction permettant de tester si le tableau est un data frame
funDataFrame <- function(Table, tableau) {
    if (!"data.frame" %in% class(Table)) {
        test <- list(verif = "ko",
                     sortie = tibble(colonne = "", ligne = "",
                                     message = paste0("Le tableau ", tableau,
                                                      " n'est pas un data.frame")))
    } else {
        test <- list(verif = "ok",
                     sortie = tibble(colonne = "", ligne = "", message = "")[0,])
    }
    
    test
}

# Fonction permettant de changer les valeurs de sortie/verif en fonction des
# resultats d'un test
funTest <- function(test, result) {
    result$verif <- siNon(test$verif == "ko", "ko", result$verif)
    
    result$sortie <- siNon(test$verif == "ko",
                           bind_rows(result$sortie,
                                     test$sortie),
                           result$sortie)
    
    result
}

# Fonction testant l'import des fichiers
funImport <- function(Table, result, empty = FALSE) {
    test <- siNon(empty, 
                  is.null(Table), 
                  any(is.null(Table), nrow(Table) == 0))
    
    out <- siNon(empty,
                 "Le fichier doit etre au bon format",
                 "Le fichier doit etre au bon format et non vide")
    if (test) {
        test <- list(verif  = "ko",
                     sortie = tibble(colonne = NA, ligne = NA, 
                                     message = out))
    } else {
        test <- initResult()
    }
    
    funTest(test, result)
}

# Fonction permettant de tester la presence de champs obligatoires
funColonnes <- function(Table, cols, result) {
    # recupere le nom de l'objet passe a l'argument Table
    tableau <- deparse(substitute(Table))
    
    test <- funDataFrame(Table, tableau)
    
    if (test$verif == "ok") {
        
        test <- which(! cols %in% colnames(Table))
        
        if (length(test) > 0) {
            test <- 
                list(verif = "ko",
                     sortie = tibble(colonne = NA, ligne = NA, 
                                     message = paste0("Les champs obligatoires",
                                                      " suivants ne sont pas ",
                                                      "presents : ", 
                                                      paste(cols[test], 
                                                            collapse = ", "))))
        } else {
            test <- list(verif  = "ok",
                         sortie = initResult())
        }
    }
    
    funTest(test, result) 
}

# Fonction retournant un commentaire pour un test donné
funCommentaire <- function(test, message, Table) {
    test <- test[sapply(test, length) > 0]
    
    if (length(test) > 0) {
        return(
            list(verif = "ko",
                 sortie = lapply(1:length(test),
                                 function(i) {
                                     tibble(colonne = 
                                                paste0("Colonne ",
                                                       names(test)[i]),
                                            ligne   =
                                                paste0("Ligne ", 
                                                       Table$ID[test[[i]]]),
                                            message = message)
                                 }) %>%
                     bind_rows()
            )
        )
    } else {
        return(list(verif = "ok", 
                    sortie = initResult()))
    }
}

# Fonction permettant de tester la presence de cellules vides
funVide <- function(Table, result) {
    tableau <- deparse(substitute(Table))
    
    test <- funDataFrame(Table, tableau)
    
    if (test$verif == "ok") {
        testEmpty <- function(x) {
            return(which(is.na(x) | x == ""))
        }
        
        test <- lapply(select(Table, -ID), testEmpty) %>%
            funCommentaire(test    = .,
                           message = "cellule vide",
                           Table   = Table)
    }
    
    funTest(test, result)
}

# Fonction permettant de tester la presence d'espace
funEspace <- function(Table, result) {
    tableau <- deparse(substitute(Table))
    
    test <- funDataFrame(Table, tableau)
    
    if (test$verif == "ok") {
        testSpace <- function(x) {
            which(grepl(" ", x))
        }
        
        test <- lapply(select(Table, -ID), testSpace) %>%
            funCommentaire(test    = ., 
                           message = "cellule avec des caracteres 'espace'",
                           Table   = Table)
    }
    
    funTest(test, result)
}

# Fonction permettant de tester si les valeurs sont numeriques
funNumerique <- function(Table, tableau = NULL, result) {
    if (is.null(tableau)) tableau <- deparse(substitute(Table))
    
    test <- funDataFrame(Table, tableau)
    
    if (test$verif == "ok") {
        testNumeric <- function(x) {
            as.character(x)  %>%
                (function(i) {
                    suppressWarnings(as.numeric(i))
                })           %>%
                is.na()      %>%
                which()
        }
        
        test <- lapply(select(Table, -ID), testNumeric) %>%
            funCommentaire(test    = .,
                           message = "cellule avec valeur non numerique",
                           Table   = Table)
    }
    
    funTest(test, result)
}

# Fonction permettant de tester si les nombres sont dans un intervalle donne
funIntervalle <- function(Table, mini, maxi, result) {
    tableau <- deparse(substitute(Table))
    
    test <- funDataFrame(Table, tableau)
    
    if (test$verif == "ok") {
        testInterval <- function(x, mini, maxi) {
            suppressWarnings(as.numeric(x)) %>%
                (function(x) {
                    (x < mini | x > maxi) %>%
                        which()
                })
        }
        
        test <- funNumerique(Table, tableau, initResult())       %>%
            funTest(test = lapply(select(Table, -ID),
                                  testInterval, mini, maxi) %>%
                        funCommentaire(test    = .,
                                       message = 
                                           paste0("cellule avec valeur ",
                                                  "inferieure a ", mini,
                                                  " ou superieure a ", maxi),
                                       Table   = Table),
                    result = .)
    }
    
    funTest(test, result)
}

# Fonction permettant de tester si les nombres sont entiers
funEntier <- function(Table, result) {
    tableau <- deparse(substitute(Table))
    
    test <- funDataFrame(Table, tableau)
    
    if (test$verif == "ok") {
        testInteger <-   function(x) {
            suppressWarnings(as.numeric(x)) %>%
                (function(x) abs(x - round(x)) > 
                     .Machine$double.eps^0.5) %>%
                which()                       %>%
                return()
        }
        
        test <- funNumerique(Table, tableau, initResult())         %>%
            funTest(test = lapply(select(Table, -ID), testInteger) %>%
                        funCommentaire(test    = ., 
                                       message = paste0("cellule avec valeur ",
                                                        "non entiere"),
                                       Table   = Table),
                    result = .)
        
    }
    
    funTest(test, result)
}

# Fonction permettant de tester si les nombres sont positifs
funPositif <- function(Table, result) {
    tableau <- deparse(substitute(Table))
    
    test <- funDataFrame(Table, tableau)
    
    if (test$verif == "ok") {
        testPositive <- function(x) {
            x <- suppressWarnings(as.numeric(x))
            
            which(x != abs(x))
        }
        
        test <- funNumerique(Table, tableau, initResult()) %>%
            funTest(test = lapply(select(Table, -ID), testPositive) %>%
                        funCommentaire(test    = .,
                                       message = "cellule avec valeur non positive",
                                       Table   = Table),
                    result = .)
    }
    
    funTest(test, result)
}

# Fonction permettant de tester le format des dates
funDate <- function(Table, dateFormat, result) {
    tableau <- deparse(substitute(Table))
    
    test <- funDataFrame(Table, tableau)
    
    if (test$verif == "ok") {
        testDate <- function(x, dateFormat) {
            suppressWarnings(as.Date(x, format = dateFormat)) %>%
                is.na()                                         %>%
                which()
        }
        
        test <- lapply(select(Table, -ID),
                       testDate, dateFormat = dateFormat) %>%
            funCommentaire(test    = .,
                           message = "cellule avec format de date non valide",
                           Table   = Table)
    }
    
    funTest(test, result)
}

# Fonction permettant de tester des codes
funCodes <- function(Table, codes, codeType, result) {
    tableau <- deparse(substitute(Table))
    
    test <- funDataFrame(Table, tableau)
    
    if (test$verif == "ok") {
        testCodes <- function(x, codes) {
            return(which(! x %in% codes))
        }
        
        test <- lapply(select(Table, -ID), testCodes, codes) %>%
            funCommentaire(test    = .,
                           message = paste0("cellule avec code ", 
                                            codeType, " non valide"),
                           Table   = Table)
    }
    
    funTest(test, result)
}

# Fonction permettant de nettoyer les sorties
funSortie <- function(result) {
    if (result$verif == "ok") {
        ""
    } else {
        sortie <- result$sortie
        
        msgLevels <- c("vide", "espace", 
                       "numerique", "entiere", "positive",
                       "date", "code")
        
        messages <- tibble(message = unique(sortie$message),
                           level   = NA)
        
        for (i in 1:length(msgLevels)) {
            i2 <- which(grepl(pattern = msgLevels[i], x = messages$message))
            
            messages$level[i2] <- i
        }
        
        messages <- arrange(messages, level) %>%
            mutate(message = factor(message, levels = message))
        
        mutate(sortie,
               message = factor(message, 
                                levels = messages$message)) %>%
            group_by(colonne, ligne)                          %>%
            summarise(message = message[as.numeric(message) == 
                                            min(as.numeric(message))][1])
    }
}

## Fonction permettant de verifier la validite de l'ensemble des tests
funValid <- function(resultats) {
    sapply(resultats,
           function(x) {
               x$verif == "ko"
           }) %>%
        (function(x) siNon(any(x), "ko", "ok"))
}

## Fonction permettant d'ecrire le fichier de sortie
funResult <- function(indic, vIndic, heure_debut, valid, 
                      sortie_envir, sortie_faune) {
    # determination du temps de calcul
    heure_fin <- Sys.time()
    heure_dif <- heure_fin - heure_debut
    temps_execution <- paste(round(heure_dif, 2),
                             attr(heure_dif, "units"),
                             sep = "")
    
    # creation du bandeau d'information
    etiquette <- paste(indic, vIndic, Sys.Date(),
                       "Temps d'execution :", temps_execution)
    
    # information de validation ou non des donnees d'entree
    print(valid)
    
    # sortie du bandeau d'information
    write(etiquette, file = "", sep = "")
    
    # sortie du rapport d'erreur de l'indice
    write("Fichier de donnees environnementales: ",
          file = "", sep = "")
    write.table(sortie_envir,
                sep = ";", col.names = FALSE,
                row.names = FALSE, quote = FALSE)

    write("Fichier de donnees faunistiques: ",
          file = "", sep = "")
    write.table(sortie_faune,
                sep = ";", col.names = FALSE,
                row.names = FALSE, quote = FALSE)
    
    # creation du fichier de sortie avec bandeau d'information
    outputFile <- paste0(indic, "_", vIndic, "_rapport_erreur.csv")
    
    cat(etiquette, file = outputFile, append = FALSE)
    cat("\n", file = outputFile, append = TRUE)
    
    # completion du fichier de sortie avec les erreurs
    cat("Fichier de donnees environnementales:\n",
        file = outputFile, append = TRUE)
    
    write.table(sortie_envir,
                file = outputFile, sep = ";",
                col.names = FALSE, row.names = FALSE,
                quote = FALSE, append = TRUE)
    
    cat("Fichier de donnees faunistiques:\n",
        file = outputFile, append = TRUE)
    
    write.table(sortie_faune,
                file = outputFile, sep = ";",
                col.names = FALSE, row.names = FALSE,
                quote = FALSE, append = TRUE)
    
}

## INITIALISATION DU TRAITEMENT ----

# Ne pas afficher les messages d'avis
options(warn = -1, show.error.messages = FALSE)

# Recuperation des fichiers d'entree
File_envir <- "IPR_entree_01_env.txt"
File_faune <- "IPR_entree_02_faun.txt"

# Initialisation de l'heure
heure_debut <- Sys.time()

## IMPORT DES FICHIERS ----

# Import des fichiers d'entree
data_envir	<- NULL
data_faune	<- NULL

data_envir <- read.table(File_envir,
                       header = TRUE, sep = "\t",
                       stringsAsFactors = FALSE,
                       colClasses = c(CODE_OPERATION = "character",
                                      CODE_STATION   = "character")) %>% 
    mutate(ID = seq(n()) + 1)

data_faune	<- read.table(File_faune,
                       header = TRUE, sep = "\t",
                       stringsAsFactors = FALSE,
                       colClasses = c(CODE_OPERATION = "character")) %>% 
    mutate(ID = seq(n()) + 1)

## VALIDATION DES DONNEES ----

# Analyse du fichier environnemental
resultat_envir <- initResult() %>%
    funImport(
        Table  = data_envir,
        empty  = FALSE,
        result = .)            %>%
    funColonnes(
        Table  = data_envir,
        cols   = c("CODE_OPERATION", "CODE_STATION", "DATE",
                   "SURF", "SBV", "DS", "LAR", "PENT", "PROF",
                   "ALT", "Tjanvier", "Tjuillet", "BASSIN"),
        result = .)

if (resultat_envir$verif == "ok") {
    resultat_envir <- resultat_envir %>%
        funVide(
            Table    = select(data_envir,
                              ID, CODE_OPERATION, SURF, SBV, DS, LAR, PENT,
                              PROF, ALT, Tjanvier, Tjuillet, BASSIN),
            result   = .)            %>%
        funEspace(
            Table    = select(data_envir,
                              ID, SURF, SBV, DS, LAR, PENT,
                              PROF, ALT, Tjanvier, Tjuillet, BASSIN),
            result   = .)            %>%
        funPositif(
            Table    = select(data_envir,
                              ID, SURF, SBV, DS, LAR, PENT, PROF, ALT),
            result   = .)            %>%
        funNumerique(
            Table    = select(data_envir, ID, Tjanvier, Tjuillet),
            result   = .)            %>%
        funCodes(
            Table    = select(data_envir, ID, BASSIN),
            codes    = paste0("H", 1:9),
            codeType = "bassin",
            result   = .)
}

# Analyse du fichier faunistique
resultat_faune <- initResult() %>%
    funImport(
        Table  = data_faune,
        empty  = FALSE,
        result = .)            %>%
    funColonnes(
        Table  = data_faune,
        cols   = c("CODE_OPERATION", "CODE_TAXON", "RESULTAT"),
        result = .
    )

if (resultat_faune$verif == "ok") {
    resultat_faune <- resultat_faune %>%
        funVide(
            Table    = select(data_faune, 
                              ID, CODE_OPERATION, CODE_TAXON, RESULTAT),
            result   = .)            %>%
        funEspace(
            Table    = select(data_faune, ID, CODE_TAXON, RESULTAT),
            result   = .)            %>%
        funCodes(
            Table    = select(data_faune, ID, CODE_TAXON),
            codes    = expand.grid(LETTERS, LETTERS, LETTERS) %>%
                apply(X = ., MARGIN = 1, FUN = paste, collapse = ""),
            codeType = "taxon",
            result   = .)            %>%
        funEntier(
            Table    = select(data_faune, ID, RESULTAT),
            result   = .)            %>%
        funPositif(
            Table    = select(data_faune, ID, RESULTAT),
            result   = .)
}

# Parametre de succes/echec de la validation
valid <- funValid(resultats = list(resultat_envir, resultat_faune))

# SORTIE DU RAPPORT D'ERREUR ----
sortie_envir <- funSortie(resultat_envir)
sortie_faune <- funSortie(resultat_faune)

funResult(indic        = indic,
          vIndic       = vIndic,
          heure_debut  = heure_debut,
          valid        = valid,
          sortie_envir = sortie_envir,
          sortie_faune = sortie_faune)
