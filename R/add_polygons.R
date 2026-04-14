#' Load a package geo file
#'
#' in the package geo files are stored as Rdata with three objects
#' gis_coordinates (geomitries), gis_data (everything except for geometries)
#' and readme (list of further information)
#'
#' @param region The region of Geo data as defined in the package extdata folder
#' @param Rdata_file The name of the file (without ".RData")
#'
#' @export
#'
load_geo <- function(region, Rdata_file){
  load(file.path(
    system.file(package = "qsimVis"),
    paste0(
      "extdata/", region, "_data/", Rdata_file, ".RData")
  ))
  rm("region", "Rdata_file")
  mget(ls())
}

#' #' Add Berlin districts to a map
#' #'
#' #' Polygons are drawn in lightgray
#' #'
#' #' @importFrom graphics polygon
#' #' @export
#' #'
#' add_districts <- function(){
#'   geo <- load_geo(region = "berlin", Rdata_file = "berlin_boarder")
#'   polygon(
#'     x = geo$gis_coordinates[,1],
#'     y = geo$gis_coordinates[,2],
#'     col = "gray80")
#'
#' }
