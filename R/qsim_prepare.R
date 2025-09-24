#' Read Qsim output and reshape data for further processing
#'
#' This function combines the Qsim site information of the first 3 columns
#' to unique site names and spreads the Qsim output table for the defined
#' parameter into a table with one time column and several site columns
#'
#' @param qsim_output_file filename (including path) in which the output from
#' Qsim is listed. The format of the output file needs to be a ; - seperated
#' csv with "," as a decimal separator.
#' @param parameter_name A String defining the column name of the parameter to
#' be visualized. "VO2" is the default (Oxygen concentration).
#' @param date_column_name,id_column_name,km_column_name A String defining the
#' obligatory column names.
#' @param flow_column_name,section_column_name String defining the column names.
#' If NULL, the columns will be filled by NA values (flow column) or empty
#' chracaters (section column)
#'
#' @details
#' The flow column needs to be in m³/s
#'
#'
#' @return
#' A list of 2 tables, where the number of rows correspond to the number if
#' timepstamps and the number of columns corresponds to the number of sites plus
#' 1 timestamp column. Table 1 contains information about the defined parameter.
#' Table 2 contains flow information if available.
#'
#' Both tables are sorted by the timestamp which are in ETC/GMT+1 (Winter time
#' in central Europe)
#'
#' @importFrom data.table fread
#' @importFrom utils unstack
#'
#' @export
#'
QSIM_prepare <-function(
    qsim_output_file,
    parameter_name = "VO2",
    date_column_name = "date",
    id_column_name = "ID",
    km_column_name = "km",
    flow_column_name = "Q",
    section_column_name = NULL
){

  df_in <- data.table::fread(
    file = qsim_output_file,
    sep = ";",
    dec = ",",
    header = TRUE
  )

  if(any(!(
    c(date_column_name, parameter_name, id_column_name, km_column_name) %in% colnames(df_in)))
  ){
    stop("Please specify correct column names for:
         ID, River km, Date and Parameter.")
  }

  if(!is.null(flow_column_name) & !(flow_column_name %in% colnames(df_in))){
    warning("No Flow column called '", flow_column_name ,"' found. Values set to NA.")
  }
  if(!is.null(section_column_name) & !(section_column_name %in% colnames(df_in))){
    warning("No Flow column called '", flow_column_name ,"' found. Sections defined as empty character.")
  }

  df_in <- data.frame(
    "ID" = df_in[[id_column_name]],
    "section" = ifelse(
      is.null(section_column_name),
      yes = rep("", nrow(df_in)),
      no = df_in[[section_column_name]]),
    "km" = df_in[[km_column_name]],
    "date" = df_in[[date_column_name]],
    "para" = df_in[[parameter_name]],
    "Q" = ifelse(
      is.null(flow_column_name),
      yes = rep(NA, nrow(df_in)),
      no = df_in[[flow_column_name]])
  )

  df_in$site <- paste(
    df_in[["ID"]],df_in[["section"]], df_in[["km"]],
    sep = "__"
  )

  df_pro <- df_in[,c("date", "site", "Q", "para")]

  d_format <- findDateFormat(chracterDates = df_pro$date)

  # dots_list <- daysOfTimeShift(all_dates = df_pro$date)

  del <- which(duplicated(df_pro))
  if(length(del)){
    cat("Found and deleted duplicates.")
    print(lapply(df_pro[del,], function(x){summary(as.factor(x), maxsum = 10)}))
    df_pro <- df_pro[-del,]
  }

  dups <- which(duplicated(df_pro[,c("date", "site")]))
  if(length(dups)){
    warning(
      "Found and deleted duplicated time and site information with different parameter values.
            Please check data according to the displayed details.")
    print(paste(length(dups) * 2, "rows with same time and site information but",
                "different parameter values. The following list shows only the duplicated",
                "second part"))
    print(lapply(df_pro[dups,],function(x){summary(as.factor(x), maxsum = 10)}))
    df_pro <- df_pro[-dups,]
  }

  rows_per_site <- summary(as.factor(df_pro$site), maxsum = Inf)
  timestamps <- unique(df_pro$date)

  if(!all(rows_per_site == length(timestamps))){
    stop("Missing timestamps for one or more sites.")
  }

  df_para <- unstack(x = df_pro, para ~ site)
  df_flow <- unstack(x = df_pro, Q ~ site)

  # European winter time (continues in summer)
  posixDateTime <- as.POSIXct(
    x = unique(df_pro$date),
    format = d_format,
    tz = "Etc/GMT-1")

  output <- lapply(list(df_para, df_flow), function(x){
    y <- cbind(posixDateTime, x)
    y[order(y$posixDateTime),]
  })

  names(output) <- c("para", "flow")
  output
}

#' Read Qsim output from multiple sub-files of one model
#'
#' This function executes the function [QSIM_prepare()] multiple times and
#' combines the tables.
#'
#' @param qsim_output_file_list A list of Qsom output files (including path).
#' The format of the output files need to be  a csv, ";" - seperated
#' and "," as a decimal separator (German Style).
#' @param parameter_name A String defining the column name of the parameter to
#' be visualized. "VO2" is the default (Oxygen concentration).
#' @param date_column_name,id_column_name,km_column_name A String defining the
#' obligatory column names.
#' @param flow_column_name,section_column_name String defining the column names.
#' If NULL, the columns will be filled by NA values (flow column) or empty
#' chracaters (section column)
#'
#' @return
#' A list of 2 tables, where the number of rows correspond to the number if
#' timestamps and the number of columns corresponds to the number of sites plus
#' 1 timestamp column. Table 1 contains information about the defined parameter.
#' Table 2 contains flow information if available.
#'
#' Both tables are sorted by the timestamp which are in ETC/GMT+1 (Winter time
#' in central Europe)
#'
#' @export
#'
QSIM_prepare_multiple <-function(
    qsim_output_file_list,
    parameter_name = "VO2",
    date_column_name = "date",
    id_column_name = "ID",
    km_column_name = "km",
    flow_column_name = "Q",
    section_column_name = NULL){

  df_list <- lapply(
    qsim_output_file_list, QSIM_prepare,
    parameter_name = parameter_name,
    date_column_name = date_column_name,
    id_column_name = id_column_name,
    km_column_name = km_column_name,
    flow_column_name = flow_column_name,
    section_column_name = section_column_name
  )

  all_t <- lapply(df_list, function(x){
    x$para$posixDateTime
  })

  all_same <- all(sapply(seq_along(all_t)[-1], function(i){
    identical(x = all_t[[i-1]], y = all_t[[i]])
  }))

  if(!all_same){
    stop("Data frames cannot be merged. Different timestamps from input files")
  }

  all_para <- lapply(df_list, function(x){
    x$para[,-grep("posixDateTime", colnames(x$para))]
  })

  all_flow <- lapply(df_list, function(x){
    x$flow[,-grep("posixDateTime", colnames(x$flow))]
  })

  if(all_same){
    list("para" = cbind(
      "posixDateTime" = all_t[[1]],
      do.call(cbind, all_para)),
      "flow" = cbind(
        "posixDateTime" = all_t[[1]],
      do.call(cbind, all_flow))
    )
  }
}

