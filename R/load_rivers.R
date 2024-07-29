#' Reads river data
#'
#' Reads all csv files starting with "river" in a folder and combines it in a
#' list. Each csv defines a river stretch in the region of interest and must at
#' least contain 5 columns:
#' x: longitude
#' y: lattitude
#' distance_to_neighbour: distance to the previous point of the river in km
#' km: distance from the river estuary
#' qsim_id: The qsim ID as given by function [QSIM_prepare()] if the point
#' matches a qsim output site
#'
#' @param aggregated_data A dataframe created by one of [deviating_hours()],
#' [adverse_deviation_from_reference()] or [critical_events()] with verknet IDs
#' as rownames
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
#'  * "river_size": Information about the line width to be drawn from 1 (thin)
#'  to 3 (big).
#' @param path_manual A path to manually added river tables. If NULL (default)
#' no manually added rivers are assumed.
#'
#' @export
#'
load_rivers <- function(aggregated_data, translation_table, path_manual = NULL){
  aggregated_data <- qsimVis::add_qsimVis_id(
    aggregated_data = aggregated_data,
    translation_table = translation_table)

  path_verknet <- system.file(package = "qsimVis", "extdata/verknet")
  verknet_rivers <- available_rivers(path = path_verknet)
  manual_rivers <- if(!is.null(path_manual)){
    available_rivers(path = path_manual)
  } else {
    NULL
  }

  rivers_needed <- unique(aggregated_data$qsimVis_river)
  available <- rivers_needed %in% c(verknet_rivers, manual_rivers)

  if(sum(!available) > 0){
    cat("No river course available for:", paste0("\n", rivers_needed[!available]))
  }

  verknet_files <- verknet_rivers[verknet_rivers %in% rivers_needed]
  rivers <- lapply(verknet_files, function(file){
    river <- read.table(
      file = file.path(path_verknet, paste0(file, ".csv")),
      header = T,
      sep = ";",
      dec = "."
    )
    river$qsim_site <- ""
    river
  })
  names(rivers) <- verknet_files

  if(!is.null(path_manual)){
    manual_files <- manual_rivers[manual_rivers %in% rivers_needed]
    n_verknet <- length(rivers)
    rivers <- c(
      rivers,
      lapply(manual_files, function(file){
        river <- read.table(
          file = file.path(path_manual, paste0(file, ".csv")),
          header = T,
          sep = ";",
          dec = "."
        )
        river$qsim_site <- ""
        river
    })
    )
    names(rivers)[-(1:n_verknet)] <- manual_files
  }

  for(i in 1:nrow(aggregated_data)){
    qsimVis_river <- aggregated_data$qsimVis_river[i]
    qsim_km <- aggregated_data$km[i]
    if(qsimVis_river %in% names(rivers)){
    site_distance <- abs(rivers[[qsimVis_river]]$km - qsim_km)
    nearest_location <- which(site_distance == min(site_distance))
    rivers[[qsimVis_river]]$qsim_site[nearest_location] <-
      paste0(aggregated_data$river_name[i], "_",
            aggregated_data$section_id[i], ".",
            aggregated_data$section_name[i], "_",
            aggregated_data$km[i])
    }
  }
  rivers
}
