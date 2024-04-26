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
#' @param sixBreaks Breaks defining the lower limits of the categories.
#' @param NA_processing Either "interpolation" or "continuing".
#' Defines if NA values between two sides are either interpolated (default) or
#' kept constant based on an upstream value.
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
    rivers, river_id, aggregated_data, varName, sixBreaks,
    NA_processing = "interpolation"
){

  river_table <- rivers[[river_id]]
  # river table needs to be ordered by river km
  river_table <- river_table[order(river_table$km),]

  MisaColor <- NULL
  data("MisaColor", envir = environment())

  river_table[["value"]] <- NA
  # filter results for river id
  data_table <- aggregated_data[aggregated_data$verknet_river == river_id &
                                  !is.na(aggregated_data$verknet_river),]

  # apply results to closest verknet node, if not already defined
  km_verknet <- river_table$km
  for(i in seq_len(nrow(data_table))){
    km_result <- data_table$km[i]
    km_diff <- abs(km_result - km_verknet)
    node_match <- which(km_diff == min(km_diff))
    single_node_match <- node_match[which(is.na(river_table$value[node_match]))]
    if(length(single_node_match) == 1L){
      river_table$value[single_node_match] <- data_table[[varName]][i]
    } else if(length(single_node_match) > 1L){
      river_table$value[single_node_match[1]] <- data_table[[varName]][i]
    }
  }

  # if the last and the first verknet node are not defined, use the closest
  # data node, depennding on distance
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
      round(interpolate_multipleNA(
        data_vector = river_table$value,
        max_na = 1000,
        diff_x = river_table$distance_to_neighbour)[[1]], 1)
    } else if(NA_processing == "continuing"){
      insert_downstreamNA(data_vector = river_table$value)
    }

  river_table$quality <-
    cut(river_table$value, breaks = sixBreaks,
        include.lowest = TRUE, ordered_result = TRUE)

  river_table$color <- MisaColor[as.numeric(river_table$quality)]
  river_table
}
