#' Filters the parameter data frame by time and sites
#'
#' @description
#' Filters the loaded data frame by sites and time
#'
#' @param dataFrame Data frame loaded by a MiSa function (see details)
#' @param sites Vector of strings containing the names of considered sites,
#' as column names of the data frame. If "" (default) all sites are included
#' @param tBeg POSIX-Value with a start time of the observeration interval
#' @param tEnd POSIX-Value with an end time of the observeration interval
#'
#' @details
#' The name of the site column must be "site", the name of the timestamp column
#' must be "posixDateTime".
#'
#' @return
#' A filtered data frame with the same columns as the input data frame
#'
#' @export
#'
filter_parameter_data <- function(
    dataFrame, sites = "", tBeg = min(dataFrame$posixDateTime, na.rm = TRUE),
    tEnd = max(dataFrame$posixDateTime, na.rm = TRUE)
){
  if(!("" %in% sites)){
    dataFrame <- dataFrame[dataFrame$site %in% sites, ]
  }
  if(!is.null(tBeg)){
    dataFrame <- dataFrame[dataFrame$posixDateTime >= tBeg, ]
  }
  if(!is.null(tEnd)){
    dataFrame <- dataFrame[dataFrame$posixDateTime <= tEnd, ]
  }
  dataFrame
}
