#' Loads a data table containing time frames for events of interest
#'
#' @param filename file name (including path) of the event definition table
#'
#' @return
#' Table as defined in 'extdata/misa_data' filtered
#'
#' @export
#' @importFrom utils read.table
load_timeframes <- function(filename){
  e_data <- read.table(
    file = filename,
    header = TRUE,
    sep = ";",
    dec = "."
  )

  e_data$tBeg <- as.POSIXct(e_data$tBeg, format = "%d.%m.%Y %H:%M")
  e_data$tEnd <- as.POSIXct(e_data$tEnd, format = "%d.%m.%Y %H:%M")

  e_data
}
