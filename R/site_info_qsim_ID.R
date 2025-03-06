#' Splits the Qsim ID into 4 differnt informations
#'
#' @param qsim_id A character that is a combination of
#' river, section_id, section_name and kilometer
#'
#' @details
#' The format of the qsim_id must be "RRR_SSSS.rrrr_xx.x"
#' where RRR is a character string for the river name, SSSS is a character for
#' the section id, rrrr is a character string for section name and  xx.x is a
#' numeric value of kilometers. The length of character strings may differ.
#'
#' @return A list of 4, including river name, section id, section name and km
#'
#' @export
#'
site_info_from_qsimID<- function(qsim_id){
  s1 <- strsplit(qsim_id, split = "__")[[1]]
  s2 <- strsplit(s1[2], "\\.")[[1]]
  list("river_name" = s1[1],
       "section_id" = s2[1],
       "section_name" = s2[2],
       "km" = as.numeric(s1[3]))
}

#' Adds site info based on qsimd Ids to a table
#'
#' @param df_in A Dataframe containing site specific data in rows
#' @param v_qsim_ids A vector of qsim ids with the same length as the number of
#' rows of df_in.
#'
#' @return
#' The dataframe df_in extented 4 columns that contain site information
#'
#' @export
#'
add_site_info <- function(df_in, v_qsim_ids){
  site_infos <- lapply(v_qsim_ids, site_info_from_qsimID)
  site_infos <- lapply(site_infos, as.data.frame)
  df_infos <- do.call(rbind, site_infos)
  cbind(df_in,  df_infos)
}
