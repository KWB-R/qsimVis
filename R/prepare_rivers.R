#' Load river stretches, merge sites, add simulation result and fill gaps
#'
#' @param mapping_table A table that maps the Qsim sites to the verknet and
#' manually added river names.
#' @param aggregated_data A dataframe created by one of [deviating_hours()],
#' [adverse_deviation_from_reference()] or [critical_events()] with verknet IDs
#' as rownames
#' @param path_manual A path to manually added river tables. If NULL (default)
#' no manually added rivers are assumed.
#' @param value_column The column name of the aggregated data table to be
#' plotted
#' @param gap_filling Either "interpolation" or "steps".
#' Defines if NA values between two locations are either interpolated (default)
#' or kept constant based on an upstream value.
#'
#' @return A list of river stretches, including values from the simulation
#'
#' @export
#'
prepare_rivers <- function(
    mapping_table,
    aggregated_data,
    value_column,
    path_manual = NULL,
    gap_filling = "steps"
){
  aggregated_data <- qsimVis::add_qsimVis_id(
    aggregated_data = aggregated_data,
    translation_table = mapping_table)

  rivers <- load_rivers(
    qsimVis_IDs = aggregated_data$qsimVis_ID,
    path_manual = path_manual
  )

  rivers_ext <- lapply(
    X = names(rivers), FUN = qsimVis::extend_riverTable,
    rivers = rivers,
    aggregated_data = aggregated_data,
    varName = value_column,
    NA_processing = gap_filling)
  names(rivers_ext) <- names(rivers)

  rivers_ext <- lapply(names(rivers), function(r){
    list("data" = rivers_ext[[r]],
         "pp" = list( # list of plot properties
           "river_lwd" = mapping_table$size_type[mapping_table$qsimVis_ID == r])
    )
  })
  names(rivers_ext) <- names(rivers)
  rivers_ext
}
