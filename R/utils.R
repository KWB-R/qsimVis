#' Get River, Section and location information from Qsim IDs
#'
#' @param assessment_list The assessment of a Qsim simulation
#'
site_info_from_qsimID<- function(assessment_list){
  lapply(assessment_list, function(x){
    if(is.data.frame((x))){
      x[["river"]] <-  sapply(rownames(x), function(NAME){
        strsplit(NAME, split = "_")[[1]][1]})

      x[["section"]] <-  sapply(rownames(x), function(NAME){
        strsplit(NAME, split = "_")[[1]][2]})

      x[["km"]] <-  as.numeric(sapply(rownames(x), function(NAME){
        strsplit(NAME, split = "_")[[1]][3]}))
      x[order(x$km, decreasing = T),]
    }
  })
}

#' Calculates x- and y-scale based on longitude and lattitude data
#'
#' Get the right scale for plotting a map. While width must be defined,
#' the according height is calculated
#'
#' @param xlim Minimum and maximum longitude of the map
#' @param ylim Minimum and maximum lattitude of the map
#' @param width Numeric value defining the width of the plot (in inch)
#'
#' @details
#' The ratio between width and height is directly based on the ratio between
#' lonigtude and lattitude. In order to get the correct scale for the plot the
#' margins need to be set to 0 (see example).
#'
#' @return
#' Named vector of two, containing the width (for x-axis dimension) and the
#' height (for y-axis dimension).
#'
#' @export
#' @importFrom geosphere distHaversine
#'
#' @examples
#' xlim <- c(13.18, 13.472)
#' ylim <- c(52.46, 52.57)
#' plotDim <- getDimensions(xlim = xlim, ylim = ylim, width = 10)
#' plotDim
#'
#' dev.new(noRStudioGD = TRUE, width = plotDim[1], height = plotDim[2])
#' par(mar = c(0, 0, 0, 0)) # no margins outside of the plot
#' plot(x = xlim, y = ylim, type = "l")
#' text(x = mean(xlim), y = ylim[2],
#'      labels = "Correctly Scaled Line",
#'      pos = 1, cex = 3)
#'
getDimensions <- function(xlim, ylim, width = 10){
  x_dist <- geosphere::distHaversine(
    p1 = c(xlim[1], ylim[1]),
    p2 = c(xlim[2], ylim[1])) / 1000 # in km

  y_dist <- geosphere::distHaversine(
    p1 = c(xlim[2], ylim[1]),
    p2 = c(xlim[2], ylim[2])) / 1000 # in km

  c("width" = width, "height" = y_dist / x_dist * width)
}


#' Adds quality categories to the river table
#'
#' As a result, each location is linked to a water quality value.
#'
#' @param river_table Table of one of the rivers loaded with
#' [load_rivers()]
#' @param aggregated_data A dataframe created by one of [deviating_hours()],
#' [adverse_deviation_from_reference()] or [critical_events()]
#' @param varName The column name of the agregated data in the output_table
#' @param sixBreaks Breaks defining the lower limits of the categories.
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
#'
extend_riverTable <- function(
    river_table, aggregated_data, varName, sixBreaks
){
  MisaColor <- NULL
  data("MisaColor", envir = environment())

  sixBreaks <- c(sixBreaks, Inf)
  river_table$value <- NA
  aggregated_data$qsim_site <- paste(aggregated_data$river,
                                  aggregated_data$section,
                                  aggregated_data$km,
                                  sep = "_")

  for(i in seq_len(nrow(aggregated_data))){
    found <- c(which(aggregated_data$qsim_site == river_table$qsim_site[i]),
               which(rownames(aggregated_data) == river_table$qsim_site[i]))
    if(length(found) > 0L){
      river_table$value[i] <- aggregated_data[[varName]][found]
    }
  }

  river_table$value <-
    round(interpolate_multipleNA(
      data_vector = river_table$value,
      max_na = 1000,
      diff_x = river_table$distance_to_neighbour)[[1]], 1)

  river_table$quality <-
    cut(river_table$value, breaks = sixBreaks,
        include.lowest = TRUE, ordered_result = TRUE)

  river_table$color <- MisaColor[as.numeric(river_table$quality)]
  river_table
}

#' Repeating values in a row within a Vector
#'
#' Describes the Values of a vector the times they are repeated and the
#' start and end position of those values
#'
#' @param v A character, factor or numeric vector
#'
#' @details
#' Parts of this function originally come from package kwb.utils::findChanges()
#'
#' @return
#' A data frame with four columns: Value (-> listed value of the input vector),
#' Repeats (times it is repeated in a row), starts_at (start position),
#' ends_at (end position).
#'
same_inarow <- function(v){

  stopifnot(!anyNA(v))

  n <- length(v)
  if (n == 0L) {
    return(NULL)
  }
  changes_at <- which(v[-n] != v[-1L]) + 1L
  result <- data.frame(
    "starts_at" = c(1L, changes_at),
    "ends_at" = c(changes_at -  1L, n),
    "value" = v[c(1L, changes_at)],
    stringsAsFactors = FALSE
  )

  data.frame(
    Value = result$value,
    repeats = result$ends_at - result$starts_at + 1L,
    starts_at = result$starts_at,
    ends_at = result$ends_at
  )
}

