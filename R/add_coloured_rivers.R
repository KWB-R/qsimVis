#' Adds color scaled river data to a map
#'
#' @param ext_rivers A list of rivers loaded with [prepare_rivers()] and extended
#' with [value_to_classes()]
#'
#' @importFrom graphics lines
#' @export
#'
add_coloredRivers <- function(
    ext_rivers
){
  for(j in seq_along(ext_rivers)){
    lines(x = ext_rivers[[j]]$data$x, y = ext_rivers[[j]]$data$y,
          col = "steelblue", lwd = ext_rivers[[j]]$pp$river_lwd)
    for(i in seq_len(nrow(ext_rivers[[j]]$data) - 1)){
      lines(x = ext_rivers[[j]]$data$x[i:(i+1)],
            y = ext_rivers[[j]]$data$y[i:(i+1)],
            col = as.character(ext_rivers[[j]]$data$color[i+1]),
            lwd = 4 / ext_rivers[[j]]$pp$river_lwd)
    }
  }
}

