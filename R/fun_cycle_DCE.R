#' Déterminer le cycle DCE auquel les prélèvements appartiennent
#'
#' @param var_temp Variable du dataframe contenant les années


cycle_DCE <- function(var_temp){
  case_when(
    var_temp >= 2004 & var_temp <= 2012 ~ "2004-2012",
    var_temp >= 2013 & var_temp <= 2018 ~ "2013-2018",
    var_temp >= 2019 & var_temp <= 2024 ~ "2019-2024",
    TRUE ~ NA_character_
  )
}
