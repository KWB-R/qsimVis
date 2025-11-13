#' Add a logo to the plot
#'
#' The logo needs to be in the package extdata/logo folder as png file
#'
#' @param logo_filename Filename including .png ending
#' @param position One of "bottomright", "bottomleft", "bottom", "right", "left",
#' "center", "topright", "topleft" or "top".
#' @param size Numeric value defining the logo size. It is a factor applied to
#' 1/10 of the x-axis length
#' @param indent Numeric value (usually between 0 and 1) defining the indent from
#' the plot frame. like for the size this is a factor refered to 1/10 of x or y
#' dimension of the plot
#' @param bg_col The background color of the logo (NULL does not draw any
#' background). This can be an R-Color as character string or entered as
#' [rgb()], so transperancy can be included.
#'
#' @return Logo is added to the plot in the active device
#'
#' @importFrom png readPNG
#' @importFrom graphics rasterImage rect
#'
#' @export
#' @examples
#' # empty plot
#' plot(NA, xlim = c(0, 2), ylim = c(0, 4),
#'   type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = ""
#' )
#' # add logo
#' add_logo(
#'   logo_filename = "KWB_Logo.png",
#'   position = "bottomright",
#'   size = 2,
#'   indent = 0.1,
#'   bg_col = rgb(red = 0, green = 0, blue = 0, alpha = 0.5)
#' )
#'
add_logo <- function(
    logo_filename, position = "bottomright", size = 1, indent = 0, bg_col = NULL
){

  logo <- png::readPNG(source = file.path(
    system.file(package="qsimVis", "extdata/logos"),
    logo_filename), native = TRUE
  )
  logo_size <- dim(logo)
  logo_y_ratio <- logo_size[1] / logo_size[2]

  x_plot_value_range <- diff(par("usr")[1:2])
  x_plot_inch_range <- par("pin")[1]
  x_values_per_inch <- x_plot_value_range / x_plot_inch_range

  y_plot_value_range <- diff(par("usr")[3:4])
  y_plot_inch_range <- par("pin")[2]
  y_values_per_inch <- y_plot_value_range / y_plot_inch_range

  if(position %in% c("bottomright", "topright", "right")){
    # definition of dimenstion based on x scale (1/10 of plot in x dimension)
    # multiplied by a factor
    xright <- par("usr")[2] - x_plot_value_range / 10 * indent
    xleft <- xright - x_plot_value_range / 10 * size
  } else if(position %in% c("bottomleft", "topleft", "left")){
    xleft <- par("usr")[1] + x_plot_value_range / 10 * indent
    xright <- xleft + x_plot_value_range / 10 * size
  } else if(position %in% c("top", "bottom", "center")){
    x_mean <- mean(par("usr")[1:2])
    xright <-  x_mean + x_plot_value_range / 10 * size / 2
    xleft <-  x_mean - x_plot_value_range / 10 * size / 2
  }

  # calculation into relative values to get y dimensions
  x_logo_value_range <- xright - xleft
  x_logo_inch_range <-  x_logo_value_range /  x_values_per_inch
  y_logo_inch_range <- x_logo_inch_range * logo_y_ratio
  y_logo_value_range <- y_logo_inch_range * y_values_per_inch

  # x_logo_value_share <- x_logo_value_range / x_total_value_range
  # y_logo_value_share <- x_logo_value_share * logo_y_scale / device_y_scale
  # y_logo_value_range <- y_logo_value_share  * y_total_value_range

  if(position %in% c("bottomleft", "bottom", "bottomright")){
    ybottom <-  par("usr")[3] + y_plot_value_range / 10 * indent
    ytop <- ybottom + y_logo_value_range
  } else if(position %in% c("topleft", "topright", "top")){
    ytop <-  par("usr")[4] - y_plot_value_range / 10 * indent
    ybottom <- ytop - y_logo_value_range
  } else if(position %in% c("left", "right", "center")){
    y_mean <- mean(par("usr")[3:4])
    ybottom <- y_mean - y_logo_value_range / 2
    ytop <- y_mean + y_logo_value_range / 2
  }
  if(!is.null(bg_col)){
    rect(xleft, ybottom, xright, ytop, col = bg_col, border = NA)
  }

  rasterImage(logo, xleft, ybottom, xright, ytop)
}


