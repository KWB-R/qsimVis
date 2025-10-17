#' Saves the graphic in the active device either as svg or png
#'
#' @param filename The Name of the file without suffix (ex. "plot")
#' @param type The default "vector" saves the graphic as svg, everything else
#' saves as png (ex. "")
#' @param path Path where to store the file. If no path is defined, the current
#' working directory is used.
#' @param resolution One of "low", "medium" or "high" (only used for png files)
#'
#' @importFrom grDevices dev.print dev.size svg
#' @export
#'
saveActiveDevice <- function(
    filename, path = NULL, type = "vector",  resolution = "medium"
){

  if(type == "vector"){
    dev.print(
      device = svg,
      file = file.path(path, paste0(filename, ".svg")),
      width = dev.size("in")[1],
      height = dev.size("in")[2],
      pointsize = par("ps")
    )

  } else {
    res <- if(resolution == "low"){
      200
    } else if(resolution == "medium"){
      500
    } else if(resolution == "high"){
      1000
    }
    dev.print(
      device = png,
      filename = file.path(path, paste0(filename, ".png")),
      width = dev.size("in")[1],
      height = dev.size("in")[2],
      units = "in", res = res
    )
  }
}
