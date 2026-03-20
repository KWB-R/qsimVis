#' The sum of hours exceeding or falling below threshold values
#'
#' @param dataFrame Data frame with time column "posixDateTime" and parameter
#' values per site.
#' @param thresholds A vector of threshold values used for the assessment
#' @param dev_type String defining the type of deviation. "elt" for equal or
#' lower than. "lt" for lower than, "egt" for equal or greater than, "gt" for
#' greater than
#' @param relative If TRUE, a relative proportion of hours compared to the total
#' timeseries period in % is returned instead the sum of hours
#'
#' @details
#' The termporal resolution of the time series needs to be constant.
#'
#' @return Data frame with rows per site and columns per threshold
#'
#' @export
#'
deviating_hours <- function(
    dataFrame,
    thresholds = c(0.5, 1, 1.5, 2, 5),
    dev_type = "elt",
    relative = FALSE
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
      } else if(dev_type == "lt"){
        sum(x < threshold, na.rm = FALSE) * resolution
      } else if(dev_type == "gt"){
        sum(x > threshold, na.rm = FALSE) * resolution
      } else if(dev_type == "egt"){
        sum(x >= threshold, na.rm = FALSE) * resolution
      } else {
        stop("deviation type unkown")
      }
    })
  })))
  cn <- if(dev_type == "elt"){
    "equal_below"
  } else if(dev_type == "lt"){
    "below"
  } else if(dev_type == "gt"){
    "above"
  } else if(dev_type == "egt"){
    "equal_above"
  }
  colnames(df_out) <- paste(cn, thresholds, sep = "_")
  if(relative){
    df_out <- df_out / (nrow(dataFrame) * resolution) * 100
  }
  df_out <- qsimVis::add_site_info(
    df_in = df_out,
    v_qsim_ids = rownames(df_out)
  )
  df_out
}

