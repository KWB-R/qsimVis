#' Add two columns class and color
#'
#' @param river_list A list of river tables including a value column created by
#' [prepare_rivers()]
#' @param classBreaks Breaks defining the limits. The lowerst value is included
#' in the lowest class. In the remaining classes, the highest value is included.
#'
#' @return The river_dataTable extended by two columns "quality" (class) and
#' "color"
#'
#' @export
#'
value_to_classes <- function(river_list, classBreaks){

  nClasses <- length(classBreaks) - 1

  MisaColor <- NULL
  data("MisaColor", envir = environment())
  MisaColor <- MisaColor[round(seq(from = 1, to = 6, length.out = nClasses))]

  output_list <- lapply(names(river_list), function(N){

    r <- river_list[[N]]
    river_dataTable <- r$data
    river_dataTable$quality <-
      cut(river_dataTable$value, breaks = classBreaks,
          include.lowest = TRUE, ordered_result = TRUE)

    river_dataTable$color <- MisaColor[as.numeric(river_dataTable$quality)]
    list("data" = river_dataTable,
         "pp" = r$pp
    )
  })
  names(output_list) <- names(river_list)
  output_list
}

