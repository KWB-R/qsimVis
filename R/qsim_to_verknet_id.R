#' Changes Qsim Ids to Verknet IDs
#'
#'
#' @param aggregated_data A dataframe created by one of [deviating_hours()],
#' [adverse_deviation_from_reference()] or [critical_events()]
#' @param translation_table Data frame of at least two columns: column number 1:
#' Verknet ID, column with name "sim_ID": Qsim ID
#'
#' @export
#'
qsim_to_verknet_id <- function(aggregated_data, translation_table){
  separate_qsim_ids <- strsplit(x = translation_table$section_name, ",")
  qsim_ids <- unlist(separate_qsim_ids)
  aggregated_data$verknet_river <- aggregated_data$section_name
  for(qsim_id in qsim_ids){
    df_row <- which(sapply(separate_qsim_ids, function(x){qsim_id %in% x}))
    if(length(df_row) == 0L){
      stop("Qsim ID: ", qsim_id, " not found in translation table")
    }
    verknet_id <- translation_table[["Abk."]][df_row]
    aggregated_data$verknet_river <-
      gsub(
        pattern = qsim_id,
        replacement = verknet_id,
        x = aggregated_data$verknet_river
      )
  }
  aggregated_data
}
