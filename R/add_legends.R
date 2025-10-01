#' Add a legend for the river color to the plot
#'
#' @param ext_rivers A list of rivers loaded with [prepare_rivers()] and extended
#' with [value_to_classes()]
#' @param LegendTitle String with title of the legend
#' @param LegendLocation Either "top" or "right" (outside the plot margin)
#'
#' @importFrom graphics legend
#' @export
#'
add_river_legend <- function(
    ext_rivers, LegendTitle = "", LegendLocation = "right"
    ){

  if(LegendLocation == "top"){
    lx <- mean(par("usr")[1:2])
    ly <- par("usr")[4]
    xadj <- 0.5
    hor <- TRUE
  } else if(LegendLocation == "right"){
    lx <- par("usr")[2]
    ly <- par("usr")[3]
    xadj <- 0
    hor <- FALSE
    LegendTitle <- gsub(
      pattern = '(.{1,20})(\\s|$)',
      replacement = '\\1\n',
      x = LegendTitle)
  }

  x <- ext_rivers[[1]]$data
  data_type <- if("value_class" %in% colnames(x)){
    "categorical"
  } else {
    "numerical"
  }

  if(data_type == "categorical"){
    cs <- levels(x$value_class)
    cc <- levels(x$color)

    nc <- length(cs)
    if(grepl(pattern = "^\\(", x = cs[1])){
      cs[1] <- paste0("< ", strsplit(x = cs[1], split = ",")[[1]][-1])
    }

    if(grepl(pattern = "\\)$", x = cs[nc])){
      cs[nc] <- paste0(strsplit(x = cs[nc], split = ",")[[1]][1])
    }
    cs <- gsub(pattern = "\\[", replacement = "", x = cs)
    cs <- gsub(pattern = "\\(", replacement = "> ", x = cs)
    cs <- gsub(pattern = "\\,", replacement = " - ", x = cs)
    cs <- gsub(pattern = "\\]", replacement = "", x = cs)
    l_content <- cs
    legend(x = lx, y = ly, legend = cs, col = cc, lwd = 6,
           bg= "white", bty = "n", title = LegendTitle,
           xpd = T, xjust = xadj, yjust = 0, horiz = hor)

  }
  ext_rivers$BVK$data$value_class

  # ll <- length(classBreaks)
  # l_content <-
  #   if(dataType == "time"){
  #     c(paste0("<= ", classBreaks[2]), paste0("> ", classBreaks[2:(ll-1)]))
  #   } else {
  #     c(paste0("<= ", classBreaks[2:(ll-1)]), paste0(">", classBreaks[(ll-1)]))
  #   }


}
