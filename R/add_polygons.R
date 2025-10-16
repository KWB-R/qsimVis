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
#' @param plot_names If TRUE, names of the districts will be included in the plot
#' @param highlight_catchments Character vector containing the catchments names
#' of catchments to be highlighted
#' @param highlight_style Either "shaded" (default) or a specific color, which
#' cab also be defined by [rgb()]
#'
#' @importFrom graphics polygon abline text
#' @export
#'
Berlin_add_catchments <- function(
    plot_names = FALSE, highlight_catchments = NULL, highlight_style = "shaded"
){
  ezg <- NULL
  load(file.path(system.file(package = "qsimVis"),
                 "extdata/berlin_data/catch_polygon.RData"))

  if(plot_names){
    ezg_namePositions <- lapply(ezg, function(loc_df) {
      data.frame("x" = mean(loc_df[,1]), "y" =  mean(loc_df[,2]))
    })

    ezg_namePositions$`Bln II`$y <-  ezg_namePositions$`Bln II`$y - 0.01
    ezg_namePositions$`Bln III`$x <-  ezg_namePositions$`Bln III`$x + 0.01
    ezg_namePositions$`Bln III`$y <-  ezg_namePositions$`Bln III`$y - 0.003
    ezg_namePositions$`Bln IIIa`$x <-  ezg_namePositions$`Bln IIIa`$x - 0.01
    ezg_namePositions$`Bln IIIa`$y <-  ezg_namePositions$`Bln IIIa`$y - 0.04
    ezg_namePositions$`Bln VIII`$x <-  ezg_namePositions$`Bln VIII`$x + 0.005
    ezg_namePositions$`Bln VIII`$y <-  ezg_namePositions$`Bln VIII`$y + 0.005
    ezg_namePositions$`Bln IX`$x <-  ezg_namePositions$`Bln IX`$x + 0.005
    ezg_namePositions$`Bln IX`$y <-  ezg_namePositions$`Bln IX`$y + 0.005
    ezg_namePositions$`Bln IX`$y <-  ezg_namePositions$`Bln IX`$y + 0.005
    ezg_namePositions$`Bln XI`$x <-  ezg_namePositions$`Bln XI`$x - 0.013
    ezg_namePositions$`Chb I`$y <-  ezg_namePositions$`Chb I`$y - 0.005
    ezg_namePositions$`Chb Ia`$y <-  ezg_namePositions$`Chb Ia`$y + 0.02
    ezg_namePositions$`Chb Ia`$x <-  ezg_namePositions$`Chb Ia`$x - 0.08
    ezg_namePositions$`Chb III`$y <-  ezg_namePositions$`Chb III`$y + 0.002
    ezg_namePositions$`Ruh`$x <-  ezg_namePositions$`Ruh`$x + 0.015
    ezg_namePositions$`Spa1`$x <-  ezg_namePositions$`Spa1`$x - 0.008
    ezg_namePositions$`Wil`$y <-  ezg_namePositions$`Wil`$y + 0.01
  }

  if(length(highlight_catchments) > 0L){
    wrong_names <- !(highlight_catchments %in% names(ezg))
    if(any(wrong_names)){
      warning(paste(highlight_catchments[wrong_names], collapse = ", "),
              ": no defined catchment name(s) -> will not be highlighted")
    }
  }

  # colCircle <- rep(paste0("gray",c(60,70,80,90)), 10)
  # for(i in seq_along(ezg)){
  #   col <- colCircle[i]
  #   polygon(
  #     x = ezg[[i]][,1],
  #     y = ezg[[i]][,2],
  #     col = col)
  # }
  colCircle <- rep(paste0("gray",c(60,70,80,90)), 10)
  for(i in seq_along(ezg)){
    col <- colCircle[i]
    shading <- NULL
    if(names(ezg)[i] %in% highlight_catchments){
      if(highlight_style == "shaded"){
        shading <- 30
      } else {
        col <- highlight_style
      }
    }
    polygon(x = ezg[[i]][,1], y = ezg[[i]][,2], col = col, density = shading)
    if(plot_names){
      text(x = ezg_namePositions[[i]]$x, y = ezg_namePositions[[i]]$y,
           labels = names(ezg_namePositions)[i])

    }

  }

  if(plot_names){
    lines(
      x = c(ezg_namePositions$`Chb Ia`$x + 0.012, min(ezg$`Chb Ia`[,1]) + 0.004),
      y =  c(ezg_namePositions$`Chb Ia`$y - 0.002, max(ezg$`Chb Ia`[,2]) - 0.002))
    lines(
      x = c(ezg_namePositions$`Bln IIIa`$x, mean(ezg$`Bln IIIa`[,1]) + 0.0005),
      y =  c(ezg_namePositions$`Bln IIIa`$y + 0.002, mean(ezg$`Bln IIIa`[,2])))
  }
  abline(v = par("usr")[1:2])
  abline(h = par("usr")[3:4])

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

