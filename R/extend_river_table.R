#' Adds quality categories to the river table
#'
#' As a result, each location is linked to a water quality value.
#'
#' @param rivers List of rivers loaded with [load_rivers()]
#' @param river_id Character with verknet river ID that is one of the names of
#' the rivers list
#' @param aggregated_data A dataframe created by one of [deviating_hours()],
#' [adverse_deviation_from_reference()] or [critical_events()]
#' @param varName The column name of the agregated data in the output_table
#' @param NA_processing Either "interpolation" or "steps".
#' Defines if NA values between two locations are either interpolated (default)
#' or kept constant based on an upstream value.
#'
#' @details
#' The qsim_misa_table does not provide information for every location within
#' the river, since the local resolution of the qsim output differs from the
#' river course data. Values between two known river sites are interpolated.
#' The color scale cannot be changed. Since in all misa assessment parameters
#' low values refer to a good water quality, the colors range from green for low
#' values to red for high values.
#'
#' @return
#' The input river_table extended by three columns: 1) "value" containing the
#' exact value, 2) "quality" containing 6 different quality categories based on
#' the defined breaks, 3) "color" assigning th color for plotting
#'
#' @importFrom utils data
#' @export
#'
extend_riverTable <- function(
    rivers, river_id, aggregated_data, varName,
    NA_processing = "interpolation"
){

  if(!(varName %in% colnames(aggregated_data))){
    stop(varName, " is no column in 'aggregated_data'")
  }
  river_table <- rivers[[river_id]]
  # river table needs to be ordered by river km
  river_table <- river_table[order(river_table$km),]



  river_table[["value"]] <- NA
  # filter results for river id
  data_table <- aggregated_data[aggregated_data$qsimVis_ID == river_id &
                                  !is.na(aggregated_data$qsimVis_ID),]

  if(nrow(data_table) > 0L & any(!is.na(data_table[[varName]]))){
    # apply results to closest verknet node, if not already defined
    km_verknet <- river_table$km
    for(i in seq_len(nrow(data_table))){
      km_result <- data_table$km[i]
      km_diff <- abs(km_result - km_verknet)
      node_match <- which(km_diff == min(km_diff))
      # select nodes only, that were not selected before
      single_node_match <- node_match[which(is.na(river_table$value[node_match]))]
      if(length(single_node_match) == 1L){
        river_table$value[single_node_match] <- data_table[[varName]][i]
      } else if(length(single_node_match) > 1L){ # if more than one points of equal distance, use first of them
        river_table$value[single_node_match[1]] <- data_table[[varName]][i]
      }
    }

    # if the last and the first verknet node are not defined, use the closest
    # data node, depending on distance
    tolerable_distance <- 0.5 # km
    if(is.na(river_table$value[1])){
      first_value <- which(!is.na(river_table$value))[1]
      if(river_table$km[first_value] - river_table$km[1] < tolerable_distance){
        river_table$value[1] <- river_table$value[first_value]
      }
    }

    l <- nrow(river_table)
    if(is.na(river_table$value[l])){
      last_value <- rev(which(!is.na(river_table$value)))[1]
      if(river_table$km[l] - river_table$km[last_value] < tolerable_distance){
        river_table$value[l] <- river_table$value[last_value]
      }
    }

    river_table$value <-
      if(NA_processing == "interpolation"){
        interpolate_multipleNA(
          data_vector = river_table$value,
          max_na = 1000,
          diff_x = river_table$distance_to_neighbour)[[1]]
      } else if(NA_processing == "steps"){
        insert_downstreamNA(data_vector = river_table$value)
      }
  }
  river_table
}
