#' Adds colour scaled riverdata to a map
#'
#' @param rivers A list of rivers loaded with [load_rivers()]
#' @param output_table Table of aggregated data created by one of the
#' functions [critical_events()], [adverse_deviation_from_reference()] or
#' [critical_events()]
#' @param varName The column name of the agregated data in the output_table
#' @param sixBreaks Breaks defining the lower limits of the categories.
#' @param dataType Used only for the Legend. If "time" is used, it is assumed
#' that the first water quality category is between 0 and a value above 0, while
#' for the counting of "events" the first category is 0 only.
#' @param LegendTitle String with title of the legend
#' @param LegendLocation Either "top" or "right" (outside the plot margin)
#'
#' @importFrom graphics legend lines
#' @export
#'
add_coloredRivers <- function(
    rivers, output_table, sixBreaks, varName, dataType = "time",
    LegendTitle = dataType, LegendLocation ="right"
){

  ext_rivers <- lapply(seq_along(rivers), function(i) {
    extend_riverTable(
      river_table = rivers[[i]],
      output_table = output_table,
      varName = varName,
      sixBreaks = sixBreaks)
  })

  sixBreaks <- c(sixBreaks, Inf)
  MisaColor <- NULL
  data("MisaColor", envir = environment())

  for(j in seq_along(ext_rivers)){
    lines(x = ext_rivers[[j]]$x, y = ext_rivers[[j]]$y,
          col = "steelblue")
    for(i in seq_len(nrow(ext_rivers[[j]]) - 1)){
      lines(x = ext_rivers[[j]]$x[i:(i+1)],
            y = ext_rivers[[j]]$y[i:(i+1)],
            col = ext_rivers[[j]]$color[i+1],
            lwd = 6)
    }
  }

  ll <- length(sixBreaks)
  l_content <-
    if(dataType == "time"){
      c(paste0("<= ", sixBreaks[2]), paste0("> ", sixBreaks[2:(ll-1)]))
    } else {
      c(paste0("<= ", sixBreaks[2:(ll-1)]), paste0(">", sixBreaks[(ll-1)]))
    }
  if(LegendLocation == "top"){
    lx <- mean(par("usr")[1:2])
    ly <- par("usr")[4]
    xadj <- 0.5
    hor <- TRUE
  } else {
    lx <- par("usr")[2]
    ly <- par("usr")[3]
    xadj <- 0
    hor <- FALSE
  }
  legend(x = lx, y = ly, legend = l_content, col = MisaColor[seq_len(ll)], lwd = 6,
         bg= "white", bty = "n", title = LegendTitle,
         xpd = T, xjust = xadj, yjust = 0, horiz = hor)
}

