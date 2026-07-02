# Type d'algorithme : IPR
# Auteur(s)         : Delphine CORNEIL, Cedric MONDY
# Date              : 2018-10-05
# Version           : 1.0.3
# Interpreteur      : R version 3.5.1 (2018-07-02)
# Pre-requis        : Packages dplyr, tidyr
# Fichiers lies     : IPR_params_Coefficientsa.csv, IPR_params_Coefficientsb.csv, IPR_params_Especes_Contributives.csv
# Commentaires      : Indice Poisson Riviere calcule selon la norme NF T90-344
# (juillet 2011). Certains taxons ont ete regroupes et sont donc pris en compte
# dans le calcul de l'IPR : BRB vers BRE; CAS, CAG, CAD, CAA vers CAX; CCU vers
# CCO; CHP vers CHA; CMI vers CCO; GOL, GOO, GOU vers GOX; TRL, TRM vers TRF;
# VAI, VAC, VAB vers PHX; VAR vers VAN. Certaines valeurs de la norme ont fait
# l'objet d'arrondi different. Il s'agit des valeurs utilisees pour le calcul de
# la variable G, des ecarts-type y.

# Copyright 2016 Delphine CORNEIL
# Ce programme est un logiciel libre; vous pouvez le redistribuer ou le modifier
# suivant les termes de la GNU General Public License telle que publiee par la
# Free Software Foundation; soit la version 3 de la licence, soit (a votre gre)
# toute version ulterieure.
# Ce programme est distribue dans l'espoir qu'il sera utile, mais SANS AUCUNE
# GARANTIE; sans meme la garantie tacite de QUALITE MARCHANDE ou d'ADEQUATION a
# UN BUT PARTICULIER. Consultez la GNU General Public License pour plus de
# details.
# Vous devez avoir recu une copie de la GNU General Public License en meme temps
# que ce programme; si ce n'est pas le cas, consultez
# <http://www.gnu.org/licenses>.

## VERSION ----
indic  <- "IPR"
vIndic <- "v1.0.3"

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

coefficients_a	<- read.csv2("IPR_params_Coefficientsa.csv",dec=".")
coefficients_b	<- read.csv2("IPR_params_Coefficientsb.csv",dec=".")
especes_contrib	<- read.csv2("IPR_params_Especes_Contributives.csv")
especes_regroup <- as.character(especes_contrib[especes_contrib$REGROUP != "","Taxon"])

## DECLARATION DES FONCTIONS ----

## Fonction permettant de regrouper certains taxons
funRegroup	<- function(Table)
{
 Table$CODE_TAXON <- as.character(Table$CODE_TAXON)
 
 # selection des taxons a regrouper presents dans le prelevement
 taxa <- as.character(Table[Table$CODE_TAXON %in% especes_regroup,"CODE_TAXON"])
 
 # regroupement des taxons concernes
 for(tax in unique(taxa)) { Table[Table$CODE_TAXON==tax,"CODE_TAXON"] <- as.character(especes_contrib[especes_contrib$Taxon==tax,"REGROUP"]) } 
 
 # regroupement des taxons identiques par operation avec somme des effectifs
 Table <- as.data.frame(Table %>% group_by(CODE_OPERATION,CODE_TAXON) %>% summarise(RESULTAT=sum(RESULTAT)))
 
 return(Table)
}# fin de la fonction funRegroup


## Fonction permettant de calculer les ecarts Dsr des metriques d'occurence
funDSR		<- function(Table,G,V,A,T1,T2,Hn)
{
    # initialisation du vecteur contenant les DSR
    DSR         <- vector("numeric",3)
    names(DSR)  <- c("NER","NEL","NTE")
    obs         <- vector("numeric",3)
    names(obs)  <- c("NER","NEL","NTE") 
    theo        <- vector("numeric",3)
    names(theo) <- c("NER","NEL","NTE")
    
    # boucle par metrique :
    for (metrique in c("NER","NEL","NTE"))
    {
        # liste des taxons composant la metrique etudiee 
        TaxonsContrib	<- as.character(especes_contrib[which(especes_contrib[,metrique]=="O"),"Taxon"])
        
        # calcul de la somme des presences observees d'especes composant la metrique etudiee (Osr)
        Osr	<- length(intersect(TaxonsContrib,Table[,"CODE_TAXON"]))
        obs[metrique] <- Osr
        
        # initialisation du vecteur contenant les probabilites de presence predites des taxons composant la metrique etudiee
        Pi        <- vector("numeric",length(TaxonsContrib))
        names(Pi) <- TaxonsContrib
        
        # boucle par espece :
        for (espece in TaxonsContrib)
        {
            # selection des coefficients "a" de l'espece concernee
            nomcolonnes <- c(paste("a",0:10,sep=""),paste("a11",Hn,sep=""))
            P           <- coefficients_a[coefficients_a$Taxon==espece,nomcolonnes]
            
            # calcul de la probabilite de presence predite de l'espece
            Pi[espece]	<- 1 / ( 1 + exp(-1 * (P[,1] + (P[,2] * V) + (P[,3] * V^2) + (P[,4] * T1) +(P[,5] * T1^2) + (P[,6] * T2) +(P[,7] * T2^2) + (P[,8] * A) +(P[,9] * A^2) + (P[,10] * G) +(P[,11]* G^2) + P[,12])))
        }# fin de la boucle sur les especes
        
        # calcul de la valeur theorique attendue (Esr)
        Esr            <- sum(Pi)
        theo[metrique] <- Esr
        
        # calcul du denominateur
        denominateur <- sqrt(sum(Pi*(1-Pi)))
        
        # calcul de l'ecart pour la metrique d'occurence etudiee
        DSR[metrique] <- ((Osr - Esr) / denominateur)
    }# fin de la boucle sur les metriques d'occurence
    
    DSR["NTE"]	<- abs(DSR["NTE"])
    
    return(list(DSR=DSR,Obs=obs,Theo=theo,PI=Pi))
}# fin de la fonction funDSR


## Fonction permettant de calculer les ecarts D des metriques d'abondance
funD		<- function(Table,G,V,A,T1,T2,Hn,SURF)
{
    D_ab        <- vector("numeric",4)
    names(D_ab) <- c("DIT","DII","DIO","DTI")
    obs         <- vector("numeric",4)
    names(obs)  <- c("DIT","DII","DIO","DTI") 
    theo        <- vector("numeric",4)
    names(theo) <- c("DIT","DII","DIO","DTI") 
    
    for (metrique in c("DIT","DII","DIO","DTI"))
    {
        # liste des taxons composant la metrique etudiee 
        TaxonsContrib <- as.character(especes_contrib[which(especes_contrib[,metrique]=="O"),"Taxon"])
        
        # calcul des valeurs observees
        NIP	<- sum(Table[which(Table$CODE_TAXON %in% TaxonsContrib),"RESULTAT"])
        O		<- log((NIP + 1) / SURF)
        obs[metrique] <- exp(O)
        
        # selection des coefficients "b" de la metrique concernee
        nomlignes <- c(paste("b",0:10,sep=""),paste("b11",Hn,sep=""))
        B         <- coefficients_b[coefficients_b$coefficient %in% nomlignes,metrique]
        
        # calcul de la densite attendue des taxons composant la metrique
        E <- B[1] + (B[2] * V) + (B[3] * V^2) + (B[4] * T1) + (B[5] * T1^2) + (B[6] * T2) + (B[7] * T2^2) + (B[8] * A) + (B[9] * A^2) + (B[10] * G) + (B[11] * G^2) + B[12]
        theo[metrique] <- exp(E)
        
        # calcul de l'ecart pour la metrique d'abondance etudiee 
        D_ab[metrique] <- (O - E)
    }# fin de la boucle for sur les metriques d'abondance
    
    # calcul de l'ecart pour la metrique d'abondance etudiee
    D_ab[4] <- abs(D_ab[4])
    
    return(list(D_ab=D_ab,Obs=obs,Theo=theo))
}# fin de la fonction funD


## Fonction permettant de calculer les scores des metriques
funScores	<- function(DSR,D)
{
 # initialisation du vecteur contenant les scores des metriques et de l'IPR
 metriques <- vector("numeric",8)
 
 # calcul des probabilites associees aux ecarts (Dsr ou D)
 metriques[1] <- pnorm( DSR[1],0,1.0942   )     # NER
 metriques[2] <- pnorm( DSR[2],0,1.1381   )     # NEL
 metriques[3] <- pnorm(-DSR[3],0,1.4130   ) * 2 # NTE
 metriques[4] <- pnorm(-D  [1],0,1.6678821)     # DIT
 metriques[5] <- pnorm( D  [2],0,1.073697 )     # DII
 metriques[6] <- pnorm(-D  [3],0,1.5208727)     # DIO
 metriques[7] <- pnorm(-D  [4],0,0.9883947) * 2 # DTI
 
 # calcul du scores des 7 metriques
 metriques <- (- 2) * log(metriques)
 
 # calcul de la note IPR
 metriques[8] <- sum(metriques[1:7])            # IPR
 
 return(metriques)
}# fin de la fonction funScores

## Fonction permettant d'ecrire le fichier de sortie
funResult	<- function()
{
 # determination du temps de calcul
 heure_fin       <- Sys.time()
 heure_dif       <- heure_fin - heure_debut
 temps_execution <- paste(round(heure_dif,2),attr(heure_dif,"units"),sep="")
 
 # creation du bandeau d'information
 etiquette <- paste(indic, vIndic, format(Sys.Date(),"%d/%m/%Y"),"Temps d'execution :",temps_execution,sep=";")
 
 # sortie defaut
 write(etiquette,"",sep="")
 write.table(sortie_simple,row.names=FALSE,quote=FALSE,sep=";")  
 
 # sortie 2 - choix 1
 if(complementaire)
 {
     print("Fichier")
     write(etiquette,"",sep="")
     write.table(data_sortie,row.names=FALSE,quote=FALSE,sep=";")  
 }
 
 # creation du fichier de sortie avec bandeau d'information
 cat(etiquette,file=paste0(indic, "_", vIndic, "_resultats.csv"),append=FALSE)
 cat("\n"     ,file=paste0(indic, "_", vIndic, "_resultats.csv"),append=TRUE)
 
 # completion du fichier de sortie avec les resultats de l'indice IPR
 write.table(sortie_simple,paste0(indic, "_", vIndic, "_resultats.csv"),sep=";",row.names=FALSE,append=TRUE)
 
 if(complementaire)
 {
     # creation du fichier de sortie avec bandeau d'information
     cat(etiquette,file=paste0(indic, "_", vIndic, "_resultats_complementaires.csv"),append=FALSE)
     cat("\n"     ,file=paste0(indic, "_", vIndic, "_resultats_complementaires.csv"),append=TRUE)
     
     # completion du fichier de sortie avec les resultats de l'indice IPR
     write.table(data_sortie,paste0(indic, "_", vIndic, "_resultats_complementaires.csv"),sep=";",row.names=FALSE,append=TRUE)
 }
}# fin de la fonction funResult

## INITIALISATION DU TRAITEMENT ----

# Ne pas afficher les messages d'avis
options(warn = -1, show.error.messages = FALSE)

# Recuperation du fichier d'entree
File_envir	   <- "IPR_entree_01_env.txt"
File_faune	   <- "IPR_entree_02_faun.txt"
complementaire <- TRUE

# Initialisation de l'heure
heure_debut <- Sys.time()

## IMPORT DES FICHIERS ----

# Import des fichiers d'entree
data_envir <- read.table(File_envir,
                         header = TRUE, sep = "\t",
                         stringsAsFactors = FALSE,
                         colClasses = c(CODE_OPERATION = "character",
                                        CODE_STATION   = "character"))
data_faune	<- read.table(File_faune,
                         header = TRUE, sep = "\t",
                         stringsAsFactors = FALSE,
                         colClasses = c(CODE_OPERATION = "character"))

# Traitements preliminaires
	# Selection des operations communes aux deux fichiers
tableEnvironnement <- data_envir [data_envir$CODE_OPERATION  %in% data_faune$CODE_OPERATION,]
tableFaunistique   <- data_faune[data_faune$CODE_OPERATION %in% data_envir$CODE_OPERATION,]
	# Exclusion des bassins corses
tableEnvironnement <- tableEnvironnement[! tableEnvironnement$BASSIN == "H9",]
	# Exclusion des effectifs nuls
tableFaunistique <- tableFaunistique[tableFaunistique$RESULTAT != 0,]
	# Regroupement des taxons synonymes
tableFaunistique <- funRegroup(tableFaunistique)
	# Selection des taxons pris en compte dans le calcul de l'IPR
listeTaxonsRetenus <- especes_contrib[especes_contrib$REGROUP=="","Taxon"]
tableFaunistique   <- tableFaunistique[tableFaunistique$CODE_TAXON %in% listeTaxonsRetenus,]

# Initialisation du fichier de sortie
data_sortie <- data_envir[!duplicated(data_envir$CODE_OPERATION),c("CODE_OPERATION","CODE_STATION","DATE","ALT","BASSIN")]
data_sortie <- cbind(data_sortie,data.frame(G=NA,A=NA,V=NA,T1=NA,T2=NA,Effectif=NA,NER_obs=NA,NEL_obs=NA,NTE_obs=NA,DIT_obs=NA,DIO_obs=NA,DII_obs=NA,DTI_obs=NA,NER_theo=NA,NEL_theo=NA,NTE_theo=NA,DIT_theo=NA,DIO_theo=NA,DII_theo=NA,DTI_theo=NA,NER=NA,NEL=NA,NTE=NA,DIT=NA,DIO=NA,DII=NA,DTI=NA,IPR=NA,ABL=NA,ANG=NA,BAF=NA,BAM=NA,BLN=NA,BOU=NA,BRE=NA,BRO=NA,CAX=NA,CCO=NA,CHA=NA,CHE=NA,EPI=NA,EPT=NA,GAR=NA,GOX=NA,GRE=NA,HOT=NA,LOF=NA,LOT=NA,LPP=NA,OBR=NA,PCH=NA,PER=NA,PES=NA,ROT=NA,SAN=NA,SAT=NA,SPI=NA,TAN=NA,TOX=NA,TRF=NA,PHX=NA,VAN=NA))

# Traitement des operations non calculables
if(nrow(tableEnvironnement) == 0 | nrow(tableFaunistique) == 0 | length(intersect(tableEnvironnement$CODE_OPERATION,tableFaunistique$CODE_OPERATION))==0) 
{
 data_sortie[,c(4:11)] <- "indice non calculable"
 funResult()
} else
{ 
 operations_ok <- unique(intersect(tableEnvironnement$CODE_OPERATION,tableFaunistique$CODE_OPERATION))
 operations_ko <- unique(setdiff(union(data_envir$CODE_OPERATION,data_faune$CODE_OPERATION),operations_ok))
 data_sortie[data_sortie$CODE_OPERATION %in% operations_ko,4:11] <- "indice non calculable"


 ## CALCUL PAR OPERATION ----
 for (operation in operations_ok)
 {
  tableEnvironnementOp	<- tableEnvironnement[tableEnvironnement$CODE_OPERATION==operation,]
  tableFaunistiqueOp		<- tableFaunistique[tableFaunistique$CODE_OPERATION==operation,]
 
  # Determination de parametre
  SURF      <- tableEnvironnementOp[,"SURF"]
  SBV       <- tableEnvironnementOp[,"SBV"]
  DS        <- tableEnvironnementOp[,"DS"]
  LAR       <- tableEnvironnementOp[,"LAR"]
  PROF      <- tableEnvironnementOp[,"PROF"]
  ALT       <- tableEnvironnementOp[,"ALT"]
  PENT      <- tableEnvironnementOp[,"PENT"]
  Tjuillet  <- tableEnvironnementOp[,"Tjuillet"]
  Tjanvier  <- tableEnvironnementOp[,"Tjanvier"]
 
  # Calcul des 5 parametres environnementaux
  G  <- 3.015215 - (0.3472502 * log (SBV)) - (0.5436836 * log(DS))
  V  <- log(LAR) + log(PROF) + log(PENT) - log(LAR + 2 * PROF)
  A  <- log(ALT)
  T1 <- Tjuillet + Tjanvier
  T2 <- Tjuillet - Tjanvier
  Hn <- tableEnvironnementOp[,"BASSIN"]

  data_sortie[data_sortie$CODE_OPERATION==operation,c("G","V","A","T1","T2")] <- 
      c(G,V,A,T1,T2)
  
  # Calcul de l'ecart des metriques d'occurence
  occurences <- funDSR(tableFaunistiqueOp,G,V,A,T1,T2,Hn)
  Dsr        <- occurences$DSR
  
  # Calcul de l'ecart des metriques d'abondance
  abondances <- funD(tableFaunistiqueOp,G,V,A,T1,T2,Hn,SURF)
  D_ab       <- abondances$D_ab
  
  # Calcul de l'IPR et des metriques
  scores <- funScores(Dsr,D_ab)
  
  # Sauvegarde des scores des metriques et des parametres de l'IPR
  data_sortie[data_sortie$CODE_OPERATION==operation,c("NER","NEL","NTE","DIT","DII","DIO","DTI","IPR")] <- scores
  data_sortie[data_sortie$CODE_OPERATION==operation,"Effectif"]                                         <- sum(tableFaunistiqueOp[,"RESULTAT"])
  data_sortie[data_sortie$CODE_OPERATION==operation,c("NER_obs","NEL_obs","NTE_obs")]                   <- occurences$Obs  
  data_sortie[data_sortie$CODE_OPERATION==operation,c("NER_theo","NEL_theo","NTE_theo")]                <- occurences$Theo
  data_sortie[data_sortie$CODE_OPERATION==operation,c("DIT_obs","DII_obs","DIO_obs","DTI_obs")]         <- abondances$Obs
  data_sortie[data_sortie$CODE_OPERATION==operation,c("DIT_theo","DII_theo","DIO_theo","DTI_theo")]     <- abondances$Theo
  data_sortie[data_sortie$CODE_OPERATION==operation,names(occurences$PI)]                               <- occurences$PI
 }# fin de la boucle sur les operations
 

 ## SORTIE DES RESULTATS ----

 params <- c(NER = 7744, NEL = 7743, NTE = 7644, DIT = 7786, 
             DIO = 7746, DII = 7745, DTI = 7787, IPR = 7036)
 
 sortie_simple <- select(data_sortie,
                         CODE_OPERATION, CODE_STATION, DATE, ALT,
                         NER, NEL, NTE, DIT, DIO, DII, DTI, IPR) %>%
     gather(key = "LIB_PAR", value = "RESULTAT",
            -CODE_OPERATION, -CODE_STATION, -DATE) %>%
     arrange(CODE_STATION, DATE) %>%
     mutate(CODE_PAR = params[LIB_PAR]) %>%
     mutate(LIB_PAR = ifelse(LIB_PAR %in% c("IPR", "ALT"), LIB_PAR,
                             paste("Score", LIB_PAR)),
            DATE = as.Date(DATE, "%Y-%m-%d") %>%
                format("%d/%m/%Y")) %>%
     select(CODE_OPERATION, CODE_STATION, DATE, CODE_PAR, LIB_PAR, RESULTAT)
     
 funResult()
}
# Fin du script de calcul de l'IPR
