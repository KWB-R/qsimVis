#' Add a legend for the river color to the plot
#'
#' @param ext_rivers A list of rivers loaded with [prepare_rivers()] and extended
#' with [value_to_classes()]
#' @param LegendTitle String with title of the legend
#' @param LegendLocation Either "top" or "right" (outside the plot margin)
#' @param ... Additional legend layout arguments
#'
#' @importFrom graphics legend
#' @export
#'
add_river_legend <- function(
    ext_rivers, LegendTitle = "", LegendLocation = "right", ...
){

  if(LegendLocation == "top"){
    lx <- mean(par("usr")[1:2])
    ly <- par("usr")[4]
    xadj <- 0.5
    hor <- TRUE
  } else if(LegendLocation == "right"){
    diffx <- diff(par("usr")[1:2])
    diffy <- diff(par("usr")[3:4])
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

  data_type <- if(length(levels(x$value_class)) < 10){
    "categorical"
  } else {
    "numerical"
  }

  if(data_type == "categorical"){
    cs <- levels(x$value_class)
    cc <- levels(x$color)

    nc <- length(cs)
    if(grepl(pattern = "^\\(", x = cs[1])){
      cs[1] <- paste0("> ", strsplit(x = cs[1], split = ",")[[1]][-1])
    }

    if(grepl(pattern = "\\)$", x = cs[nc])){
      cs[nc] <- paste0(strsplit(x = cs[nc], split = ",")[[1]][1])
    }
    cs <- gsub(pattern = " ", replacement = "", x = cs)
    cs <- gsub(pattern = "\\[", replacement = "", x = cs)
    cs <- gsub(pattern = "\\(", replacement = "> ", x = cs)
    cs <- gsub(pattern = "\\,", replacement = " - ", x = cs)
    cs <- gsub(pattern = "\\]", replacement = "", x = cs)
    cs <- gsub(pattern = "^-Inf - ", replacement = "<= ", x = cs)
    cs <- gsub(pattern = " - Inf$", replacement = "", x = cs)
    l_content <- cs
    legend(x = lx, y = ly, legend = cs, col = cc, lwd = 6,
           bg= "white", bty = "n", title = LegendTitle,
           xpd = T, xjust = xadj, yjust = 0, horiz = hor, ...)

  } else if (data_type == "numerical"){
    if(LegendLocation == "right"){
      cs <- as.numeric(levels(x$classMean))
      # delta_y <- diff(cs)
      # res_factor <- delta_y / mean(delta_y)
      # res_factor <- c(res_factor, res_factor[length(res_factor)])

      ncs <- length(cs)
      cc <- levels(x$color)

      cs[1] <- as.numeric(strsplit(
        x = substring(levels(x$value_class)[1], first = 2),
        split = ","
      )[[1]][1])
      cs[ncs] <- as.numeric(strsplit(
        x = levels(x$value_class)[ncs],
        split = ",|\\]"
      )[[1]][2])

      recty <- 1/length(cc) / 1.2
      rectBottom <- ly

      # best_labels <- pretty(cs, n = 8)
      # atCol <- sapply(best_labels, function(x){
      #   x_diff <- abs(cs - x)
      #   which(x_diff == min(x_diff))[1]
      # })

      atCol <- floor(seq(1, ncs, length.out = 8))
      for(i in seq_along(cc)){
        rect(xleft = lx + diffx * 0.01, xright = lx + diffx * 0.05,
             ybottom = rectBottom, ytop = rectBottom + diffy * recty,
             col = cc[i], border = NA, xpd = TRUE
        )
        if(i %in% atCol){
          text(x = lx + diffx * 0.05,
               y = rectBottom,
               labels = signif(cs[i], 2),
               pos = 4,
               xpd = TRUE)
        }
        rectBottom <- rectBottom + diffy * recty
      }
      text(x = lx + diffx * 0.01,
           y = par("usr")[4],
           labels = LegendTitle,
           adj = c(0,1),
           cex = 1,
           xpd = TRUE
      )
    } else {
      message("No legend on top of plot possible for numerical scale.")
    }
  }
}
