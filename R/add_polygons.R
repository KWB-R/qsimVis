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

#' Add Berlin combined sewer catchments to a map
#'
#' Polygons are drawn in different types of gray
#'
#' @importFrom graphics polygon
#' @export
#'
add_catchments <- function(){

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

#' Add Berlin districts to a map
#'
#' Polygons are drawn in lightgray
#'
#' @importFrom graphics polygon
#' @export
#'
add_districts <- function(){
  geo <- load_geo(region = "berlin", Rdata_file = "berlin_boarder")
  polygon(
    x = geo$gis_coordinates[,1],
    y = geo$gis_coordinates[,2],
    col = "gray80")

}

#' Add Berlin boarder to a map
#'
#' Polygons is drawn in lightgray
#' @param bg_color Character string or [rgb()] for the polygon background color
#'
#' @importFrom graphics polygon
#' @export
#'
Berlin_add_boarder <- function(bg_color = "gray60"){
  geo <- load_geo(region = "berlin", Rdata_file = "berlin_boarder")
  polygon(
    x = geo$gis_coordinates[,"X"],
    y = geo$gis_coordinates[,"Y"],
    col = bg_color
  )
}

#' Add Berlin boarder to a map
#'
#' Polygons is drawn in lightgray
#' @param bg_color Character string or [rgb()] for the polygon background color
#'
#' @importFrom graphics polygon
#' @export
#'
Berlin_add_waterbodies <- function(bg_color = "lightblue"){
  geo <- load_geo(region = "berlin", Rdata_file = "berlin_waterbodies_ordnung1")
  polies <- unique(geo$gis_coordinates[,"L2"])
  for(poly in polies){
    poly_rows <- geo$gis_coordinates[geo$gis_coordinates[,"L2"] == poly,]
    polygon(
      x = poly_rows[,"X"],
      y = poly_rows[,"Y"],
      col = bg_color, border = NA
    )
  }

}

