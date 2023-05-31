#' Creates an map Plot, dimensions based on rivers and Qsim output data
#'
#' @param rivers List of data frames containing a river stretch (longitude and
#' latitude sorted in direction of flow) combined with the Qsim site ID
#'
#' @param plot_toner If TRUE a toner map is used as background
#' @importFrom grDevices png dev.off dev.new
#'
#' @importFrom graphics par
#' @import ggmap
#' @export
#'
plot_empty_map <- function(
    rivers, plot_toner = FALSE
){
  xlim <- range(unlist(
    lapply(rivers, function(x){range(x$x[x$qsim_id != ""])})
  ))
  ylim <- range(unlist(
    lapply(rivers, function(x){range(x$y[x$qsim_id != ""])})
  ))
  plotDim <- getDimensions(xlim = xlim, ylim = ylim, width = 10)
  width_factor <- plotDim[1]/plotDim[2]
  xpdDim <- 6
  dev.new(noRStudioGD = TRUE, height = 6, width = 6 * width_factor,
          units = "in")
  par(mar = c(xpdDim / 2, 0.2, xpdDim / 2 , xpdDim * width_factor - 0.2))

  plot(x = 0, y = 0,
       xaxt = "n", yaxt = "n", type = "n",
       xaxs = "i", yaxs = "i",
       xlab = "", ylab = "",
       xlim = xlim, ylim = ylim)

  if(plot_toner){
    relWidth <- diff(par("plt")[c(1,2)])
    relHeight <- diff(par("plt")[c(3,4)])
    add_left <- par("plt")[1] / relWidth * par("cxy")[1]
    add_right <- par("plt")[2] / relWidth * par("cxy")[1]
    add_top <- par("plt")[3] / relWidth * par("cxy")[2]
    add_bottom <- par("plt")[4] / relWidth * par("cxy")[2]

    bbox <- c(left = xlim[1] - add_left, right = xlim[2] + add_right,
              top = ylim[2] + add_top , bottom = ylim[1] - add_bottom)
    dev.new(noRStudioGD = TRUE, height = 6, width = 6 * width_factor,
            units = "in")
    t_map <- ggmap::get_stamenmap(bbox,maptype = "toner", zoom = 12)
    ggmap::ggmap(ggmap = t_map, extent = "device")

    # rect(xleft = xlim[2], xright = xlim[2] + add_right,
    #      ybottom = ylim[1] - add_bottom, ytop = ylim[2] + add_top,
    #      col = "white", xpd = TRUE)
  }
}


