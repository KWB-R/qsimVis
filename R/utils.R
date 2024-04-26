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

