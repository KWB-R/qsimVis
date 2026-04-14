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

#' Add Berlin boarder to a map
#'
#' Polygons is drawn in lightgray
#' @param bg_color Character string or [rgb()] for the polygon background color
#' @param frame The color of the polygon frame. If NA, no frame will be drawn.
#'
#' @importFrom graphics polygon
#' @export
#'
Berlin_add_boarder <- function(bg_color = "gray60", frame = "black"){
  geo <- load_geo(region = "berlin", Rdata_file = "berlin_boarder")
  polygon(
    x = geo$gis_coordinates[,"X"],
    y = geo$gis_coordinates[,"Y"],
    col = bg_color, border = frame
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

#' Add Points of interest in Berlin
#'
#' Polygons is drawn in lightgray
#' @param poiType One of the types defined in the poi-Table ("wwtp", "dwtp", or
#' further manually added types)
#' @param poiTitle A character describing the POI type, which is used as legend
#' text.
#' @param sw_connection If TRUE an arrow is drawn from the POI to i) the point
#' on the map defined by link_longitued and link_latitude or ii) a specified
#' outlet into or inlet from a water body. If both locations are defined, the
#' second one is used.
#' @param plotNames If TRUE, names of the POI will be plotted next to the points
#' @param lineColor,fillColor Character string or [rgb()] for the frame and fill
#' color of the POI, respectively
#' @param pch,pCex The point type (see pch in [plot()]) and size (numeric value)
#' of the  POI
#' @param dashed_connection If TRU (and sw_connection is TRUE) the connection
#' line will be a dashed line of both colors, lineColor and fillColor
#' @param legendPosition Position of the legend ("topleft" or "topright").
#' If NULL, no POI legend will be plotted.
#' @param rivers A list of rivers loaded by [prepare_rivers()]. Only needed if
#' connections to surface waters are drawn and defined by river parameters, such
#' as ID and river kilometer.
#'
#' @importFrom graphics strheight
#' @importFrom utils read.csv
#' @export
#'
#'
Berlin_add_poi <- function(
    poiType, poiTitle = "", sw_connection = FALSE, plotNames = FALSE, lineColor = "white",
    fillColor = "brown", pch = 21, pCex = 1.5, dashed_connection = TRUE,
    legendPosition = "topleft", rivers = NULL
){

  poi <- read.csv(file = file.path(
    system.file(package = "qsimVis"),
    "extdata/berlin_data/poi.csv"
  ), header = TRUE, sep = ";")

  df_plot <- poi[poi$type == poiType,]


  if(sw_connection){
    if(is.null(rivers) & any(df_plot$link_river_id != "")){
      stop("In the POI table river IDs and kilometers are given.",
           " In that case a list of rivers needs to be provided as argument to this function.")
    }
    r <- rivers

    for(i in 1:nrow(df_plot)){
      if(!is.na(df_plot$link_river_id[i]) & !is.na(df_plot$link_river_km[i])){
        sw <- df_plot$link_river_id[i]
        if(sw %in% names(r)){
          r_df <- r[[sw]]$data
          closest_location <- order(abs(r_df$km - df_plot$link_river_km[i]))[1]
          r_df[closest_location,]
          df_plot$link_longitude[i] <- r_df$x[closest_location]
          df_plot$link_latitude[i] <- r_df$y[closest_location]
        } else {
          warning(sw, " is not part of the river IDs and no connection can be drawn")
        }
      }
      if(!is.na(df_plot$link_longitude[i]) & !is.na(df_plot$link_latitude[i])){
        if(!dashed_connection){
          lines(x = c(df_plot$x_longitude[i], df_plot$link_longitude[i]),
                y = c(df_plot$y_latitude[i], df_plot$link_latitude[i]),
                col = fillColor, lwd = 2)
        } else {
          lines(x = c(df_plot$x_longitude[i], df_plot$link_longitude[i]),
                y = c(df_plot$y_latitude[i], df_plot$link_latitude[i]),
                col = lineColor, lwd = 2, lty = "dotted")
        }
      }
    }
  }
  for(i in 1:nrow(df_plot)){
    if(plotNames){
      add_POI(lat = df_plot$y_latitude[i], lon = df_plot$x_longitude[i],
              lineColor = lineColor, fillColor = fillColor,
              title = df_plot$name[i], pch = pch, cex = pCex)
    } else {
      add_POI(lat = df_plot$y_latitude[i], lon = df_plot$x_longitude[i],
              lineColor = lineColor, fillColor = fillColor,
              title = "", pch = pch, cex = pCex)
    }
  }

  if(!is.null(legendPosition)){
    n <- nTypes()

    x_plot_value_range <- diff(par("usr")[1:2])
    y_plot_value_range <- diff(par("usr")[3:4])
    x_width <- strwidth("A", cex = 1.2)
    y_width <- strheight("A", cex = 1)

    if(legendPosition == "topright"){
      xP <- par("usr")[2] - x_plot_value_range / 10 * 0.2
    } else if(legendPosition == "topleft"){
      xP <- par("usr")[1] + x_plot_value_range / 10 * 0.2
    }
    ytop <-  par("usr")[4] - y_plot_value_range / 10 * 0.2

    points(
      x = xP, y = ytop - (n -1) * y_width * 1.5,
      pch = pch, cex = pCex,
      col = lineColor, bg = fillColor, lwd = 2)

    if(legendPosition == "topleft"){
      text(
        x = xP + x_width,
        y = ytop - (n -1) * y_width * 1.5,
        labels = poiTitle,
        pos = 4)
    } else if(legendPosition == "topright"){
      text(
        x = xP - x_width,
        y = ytop - (n -1) * y_width * 1.5,
        labels = poiTitle,
        pos = 2
      )
    }
  }
}

#' Create a counter for the number of added points
#'
#' Creates a closure that keeps track of how many times it has been incremented.
#' Calling the returned function without `reset = TRUE` increases the counter by
#' one and returns the updated value. If `reset = TRUE`, the counter is reset to
#' zero.
#'
#' @return A function that increments or resets an internal counter.
#' @examples
#' counter <- qsimVis:::poiType_counter()
#' counter()      # 1
#' counter()      # 2
#' counter(TRUE)  # 0
#'
poiType_counter <- function() {
  n <- 0 # Local state inside the closure

  function(reset = FALSE) {
    n <<- if (!reset) {
      n + 1
    } else {
      0
    }
    return(n)
  }
}

#' Counter function instance
#'
#' A function created by `poiType_counter()` that keeps track of the number of
#' added points.
#' @param reset If `TRUE`, the internal counter is reset to 0. If `FALSE` (default),
#'
#' @return A function with an internal counter.
#'
nTypes <- poiType_counter()
