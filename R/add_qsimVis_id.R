#' Adds the river ID of qsimVis package to the table
#'
#' River ID can either be defined by verknet data (see details) or a manually
#' entered. The output tables lists the the source of river information and
#' the defined size of the river for plotting as additional columns.
#'
#'
#' @param aggregated_data An input dataframe. Either manually entered or created
#' by one of [deviating_hours()],[adverse_deviation_from_reference()] or
#' [critical_events()]
#' @param translation_table Project specific translation dataframe that
#' contains verknet data and project specific IDs. Column names must be
#' * "ID": Verknet ID or ID of manually added rivers which are also the
#' filnames of the river csv tables
#' * "verknet_BWaStrIdNr": The water body number in verknet. If NA the river
#' information is assumed to be added manually.
#' * "Bezeichnung": The real water body name
#' * "section_name": The project specific ID of the whole river or a river
#'  section. Can be multiple IDs per waterbody, separated by ",", however, must
#'  be unique.
#' @param id_source A string defining the part of the site name which is used
#' to find the river name in the database. This must be the ID that is given in
#' the "section_name" column of the translation table. Usually this
#' is "river_name" but it can be changed to "section_name" instead.
#'
#' @details
#' The Verknet data is available here: https://www.gdws.wsv.bund.de/DE/service/karten/03_VerkNet-BWaStr/VerkNet-BWaStr_node.html
#'
#' @return
#' A data extended aggregated_data data frame by qsimVis_river and qsimVis_source
#
#' @export
#'
add_qsimVis_id <- function(
    aggregated_data, translation_table, id_source = "river_name"
){
  separate_project_ids <- strsplit(x = translation_table$section_name, ",")
  project_ids <- unlist(separate_project_ids)

  aggregated_data$qsimVis_source <- aggregated_data$qsimVis_river <- NA

  for(project_id in project_ids){
    id_row <- which(sapply(separate_project_ids, function(x){project_id %in% x}))

    if(length(id_row) == 0L){
      stop("Project ID: ", project_id, " not found in translation table")
    }
    if(length(id_row) > 1L){
      stop("Project ID: ", project_id, " is part of two or more rivers.")
    }

    qsimVis_id <- translation_table[["ID"]][id_row]
    qsimVis_source <- ifelse(
      test = is.na(translation_table[["verknet_BWaStrIdNr"]][id_row]),
      yes = "manually_added",
      no = "verknet"
    )

    section_rows <- grep(
      pattern = paste0("^", project_id, "$") ,
      x = aggregated_data[[id_source]]
    )
    aggregated_data$qsimVis_river[section_rows] <- qsimVis_id
    aggregated_data$qsimVis_source[section_rows] <- qsimVis_source
  }
  aggregated_data
}
