#' Add polygons to a map
#'
#' Polygons are drawn in different types of gray and names are added, without
#' overlapping the catchment boundaries or any river
#'
#' @param highlight_catchments Character vector containing the catchments names
#' of catchments to be highlighted
#' @param highlight_style Either "shaded" (default) or a specific color, which
#' cab also be defined by [rgb()]
#'
#' @export
#'
add_polygons <- function(){

  ezg <- NULL
  load(file.path(system.file(package = "qsimVis"),
                 "extdata/berlin_data/catch_polygon.RData"))

  colCircle <- rep(paste0("gray",c(60,70,80,90)), 10)
  for(i in seq_along(ezg)){
    col <- colCircle[i]
    polygon(
      x = ezg[[i]][,1],
      y = ezg[[i]][,2],
      col = col)
  }
}
