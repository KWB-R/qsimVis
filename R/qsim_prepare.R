#' Read Qsim output and save or return table for aggregation and visualization
#'
#' This function combines the Qsim site information of the first 3 columns
#' to unique sitenames and spreads the Qsim output table into a table with one
#' time column and several parameter columns (one per site)
#'
#' @param qsim_output_file filename (including path) in which the output from
#' Gsim is listed
#' @param parameter_name A String defining the column name of the parameter to
#' be visualized. "VO2" is the default (Oxygen concentration).
#'
#' @details
#' Saves table as CSV File ";"-separated if parameter return_table is set to TRUE
#'
#' @return
#' The saved table contains a timestamp column in Central European Time and
#' several oxygen columns in mg/L
#'
#' @export
#' @importFrom utils read.table unstack
QSIM_prepare <-function(
    qsim_output_file,
    parameter_name = "VO2"
){

  df_in <- read.table(
    file = qsim_output_file,
    header = TRUE,
    sep = ";",
    dec = ","
  )

  df_in$site <- paste(df_in[[1]], df_in[[2]], df_in[[3]], sep = "_")
  df_in$para <- df_in[[parameter_name]]

  if(!("Q" %in% colnames(df_in))){
    stop("No Flow data availbale")
  }
  df_out <- df_in[,c("Datum", "site", "Q", "para")]


  # 1. Check if the same timestamps are available for all sites

  # 2. check if there are duplicated timestamps
  # delete timestamps if they appear twice at one site
  # (--> change from sommer to winter time)
  del <- which(duplicated(df_out[,1:2]))

  if(length(del)){
    cat("Found and deleted duplicated timestamps:", paste0("\n", df_out[del,1]))
    df_out <- df_out[-del,]
  }

  df_para <- unstack(x = df_out, para ~ site)
  df_flow <- unstack(x = df_out, Q ~ site)

  # European winter time (continues in summer)
  posixDateTime <- as.POSIXct(
    x =  unique(df_out$Datum),
    format = "%d.%m.%Y %H:%M",
    tz = "Etc/GMT-1")


  # no rows without timestamps allowed
  del <- which(is.na(df_out$Datum))
  if(length(del)){
    cat("Found and deleted rows without timestamps:", paste0("\n", del,1))
    df_out <- df_out[-del,]
  }

  output <- lapply(list(df_para, df_flow), function(x){
    y <- cbind(posixDateTime, x)
    y[order(y$posixDateTime),]
  })

  names(output) <- c("para", "flow")
  output
}
