#' Add two columns class and color
#'
#' @param river_list A list of river tables including a value column created by
#' [prepare_rivers()]
#' @param classBreaks Breaks defining the limits. The lowest value is included
#' in the lowest class. In the remaining classes, the highest value is included.
#' @param colorVector A vector of colors as HEX. By default, the MisaColors will
#' be used.
#'
#' @return The river_dataTable extended by two columns "quality" (class) and
#' "color"
#'
#' @export
#'
value_to_classes <- function(river_list, classBreaks, colorVector = NULL){

  output_list <- lapply(names(river_list), function(N){
    r <- river_list[[N]]
    x <- r$data
    x$value_class <- cut(
      x = x$value,
      breaks = classBreaks,
      include.lowest = TRUE,
      ordered_result = TRUE
    )
    list("data" = x,
         "pp" = r$pp
    )
  })
  names(output_list) <- names(river_list)

  colorVector <- classes_to_color(
    class_levels = levels(output_list[[1]]$data$value_class),
    colorVector = colorVector
  )

  output_list2 <- lapply(names(output_list), function(N){
    r <- output_list[[N]]
    x <- r$data
    x$color <- colorVector[x$value_class]
    list("data" = x,
         "pp" = r$pp)
  })
  names(output_list2) <- names(river_list)

  output_list2
}

