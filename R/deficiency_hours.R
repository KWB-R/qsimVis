#' Hours of defficienc within one event
#'
#' @param dataFrame Data frame with time column "posicDateTime and parameter
#' values per site
#' @param thresholds Oxygen threshold values used for the assessment in mg/L
#' @param dev_type String definint the type of deviation. "elt" for equal or
#' lower than, "egt" for equal or greater than.
#'
#' @return Data frane with rows per site and columns per threshold
#'
#' @export
#'
deviating_hours <- function(
    dataFrame,
    thresholds = c(0.5, 1, 1.5, 2, 5),
    dev_type = "elt"
){
  dates <- dataFrame$posixDateTime

  resolution <- as.numeric(difftime(
    time1 = dates[2],
    time2 = dates[1],
    units = "hours"))

  d <- dataFrame[,-1]

  df_out <- as.data.frame(t(apply(d, 2, function(x){
    sapply(thresholds, function(threshold) {
      if(dev_type == "elt"){
        sum(x <= threshold, na.rm = FALSE) * resolution
      } else if(dev_type == "egt"){
        sum(x >= threshold, na.rm = FALSE) * resolution
      } else {
        message("deviation type unkown")
      }
    })
  })))
  colnames(df_out) <- paste0("below_", thresholds)
  df_out
}
