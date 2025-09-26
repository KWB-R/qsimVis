#' The sum of hours exceeding or falling below threshold values
#'
#' @param dataFrame Data frame with time column "posixDateTime" and parameter
#' values per site
#' @param thresholds Threshold values used for the assessment
#' @param dev_type String defining the type of deviation. "elt" for equal or
#' lower than, "egt" for equal or greater than.
#'
#' @return Data frame with rows per site and columns per threshold
#'
#' @export
#'
deviating_hours <- function(
    dataFrame,
    thresholds = c(0.5, 1, 1.5, 2, 5),
    dev_type = "elt"
){
  dates <- dataFrame[["posixDateTime"]]

  resolution <- as.numeric(difftime(
    time1 = dates[2],
    time2 = dates[1],
    units = "hours"))

  d <- dataFrame[,-grep(pattern = "posixDateTime", x = colnames(dataFrame))]

  df_out <- as.data.frame(t(apply(d, 2, function(x){
    sapply(thresholds, function(threshold) {
      if(dev_type == "elt"){
        sum(x <= threshold, na.rm = FALSE) * resolution
      } else if(dev_type == "egt"){
        sum(x >= threshold, na.rm = FALSE) * resolution
      } else {
        stop("deviation type unkown")
      }
    })
  })))
  cn <- if(dev_type == "elt"){
    "below"
  } else if(dev_type == "egt"){
    "above"
  }
  colnames(df_out) <- paste(cn, thresholds, sep = "_")
  df_out <- qsimVis::add_site_info(
    df_in = df_out,
    v_qsim_ids = rownames(df_out)
  )
  df_out
}

