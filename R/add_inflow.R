#' Add inflow with size proportional circles to the map
#'
#' @param filname The file (including path) containing the inflow data table
#' @param varName The variable used to plot as defined as column name in the
#' table.
#' @param sizeMax The maximum value of the parameter to scale the point size.
#' If NULL it the maximum value is used, but it can be better to decrease the
#' sizeMax value in order to visualize small inflows better.
#'
#' @importFrom grDevices rgb
#'
#' @export
#'
add_inflow <- function(
    filname, varName, sizeMax = NULL
){
  inflow <- read.table(
    file = filname,
    header = TRUE,
    sep = ";",
    dec = "."
  )

  if(is.null(sizeMax)){
    sizeMax <- max(inflow[varName])
  }

  points(
    x = inflow$lon_grad + inflow$lon_min / 60 + inflow$lon_s / 3600,
    y = inflow$lat_grad + inflow$lat_min / 60 + inflow$lat_s / 3600,
    pch = 21,
    cex = sqrt(inflow$tVol_m3 /sizeMax)  * 7,
    bg = rgb(0,0,0, 0.5), col = "white"
  )

  legend(
    x = par("usr")[2],
    y = par("usr")[4],
    xpd = T,
    legend = c("1 000","25 000","50 000"),
    pch = 21,
    pt.cex = c(sqrt(1000 /sizeMax) * 7,
               sqrt(25000 /sizeMax) * 7 ,
               sqrt(50000 /sizeMax) * 7),
    title = "\u00dcberlaufvolumen in m\u00b3",
    bg = "white",
    bty = "n",
    x.intersp = 2,
    y.intersp = 2,
    pt.bg = rgb(0,0,0,0.5), # half transparent black
    col = "white"
  )
}
