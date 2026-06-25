#' Déterminer les dates de début et de fin d'une série de données satisfaisant à une condition
#'     de proximité temporelle entre observations successives
#'
#' @param df Dataframe contenant les données
#' @param var_id_site Variable d'identification du site
#' @param var_libelle_site variable contenant les libelles des sites
#' @param var_indice variable contenant les codes des indices
#'
#' @return Dataframe avec une ligne par série et des colonnes indiquant les débuts, fins et nombre d'observations
#' @export


count_index <- function(df, var_id_site, var_libelle_site ,var_indice) { 
  var_id_site <- rlang::enquo(var_id_site) 
  var_libelle_site <- rlang::enquo(var_libelle_site) 
  var_indice <- rlang::enquo(var_indice) 
  
  df %>% 
    dplyr::group_by(!!var_id_site, !!var_libelle_site, !!var_indice ) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>% 
    tidyr::pivot_wider( names_from = !!var_indice, 
                        values_from = n, 
                        names_prefix = "n_", 
                        values_fill = 0 ) }


