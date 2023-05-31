#' Writes aggregated data of one period of time into Excel file
#'
#' Each data frame of the list is converted into an excel sheet
#'
#' @param list_of_aggregated_data A named list of data frames
#' @param path Path and filename
#'
#' @importFrom writexl write_xlsx
#' @export
#'
save_as_excel <- function(list_of_aggregated_data, path){
  output <- site_info_from_qsimID(assessment_list = list_of_aggregated_data)
  writexl::write_xlsx(x = output, path = path)
}

