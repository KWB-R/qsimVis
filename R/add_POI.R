#' Add a point of interest (POI) with a labeled marker on a plot
#'
#' This function plots a "Point of Interest" (POI) at the specified latitude and longitude on the current plot.
#' It adds a colored triangular marker and an optional text label. If the label extends beyond the plot boundary,
#' its position is automatically adjusted to remain visible. The function is designed for use in spatial or map-based plotting contexts.
#'
#' @param lat,lon Numeric. Latitude (y-coordinate) and longitude (x-coordinate)
#' of the POI.
#' @param pch Point type of POI
#' @param lineColor,fillColor Character string or [rgb()] for the frame and fill
#' color of the POI, respectively
#' @param textbg Background color for text, if a title is provided
#' @param title Character. Text label to display next to the POI marker.
#' Default is an empty string.
#' @param cex A factor of the point size
#'
#' @details
#' The function calculates text dimensions to draw a rectangular background behind the label and places
#' the text and marker such that they fit within the plot region.
#' The marker is drawn using pch = 25 (downward-pointing triangle).
#'
#' @return Invisibly returns NULL. The function is called for its side effects (adding graphics to an existing plot).
#'
#' @seealso [points()], [text()], [rect()]
#'
#' @importFrom graphics strheight strwidth
#' @export
#'
add_POI <- function(
    lat, lon, pch = 25, lineColor = "white", fillColor = "blue",
    textbg  = rgb(1, 1, 1, 0.5), title = "", cex = 1.5
){
  x <- lon
  y <- lat
  if(title != ""){
    x_width <- strwidth(paste0(" ", title, " "))
    x_offset <- strwidth("M", cex = 1)
    y_width <- strheight(title, cex = 1.3)

    x_right <- x + x_width
    if(x_right > par("usr")[2]){
      pos <- 2
      rect(xleft = lon - x_width - 0.5 * x_offset, xright = lon,
           ybottom = lat - y_width / 2, ytop = lat + y_width / 2,
           col = textbg, border = NA)
    } else {
      pos <- 4
      rect(xleft = lon, xright = lon + x_width + 0.5 * x_offset,
           ybottom = lat - y_width / 2, ytop = lat + y_width / 2,
           col = textbg, border = NA)
    }
    text(x = lon, y = lat, pos =  pos, labels = title)
  }
  points(
    x = lon, y = lat,
    pch = pch, cex = cex,
    col = lineColor, bg = fillColor, lwd = 2)
}
