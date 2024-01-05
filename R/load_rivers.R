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
#'
#' @export
#'
load_rivers <- function(aggregated_data){
  path <- system.file(package = "qsimVis", "extdata/verknet")
  files <- dir(path, full.names = TRUE)
  filenames <- dir(path, full.names = FALSE)

  rivers_needed <- unique(aggregated_data$verknet_river)

  rivers_available <- unique(
    sapply(filenames, function(x){
      strsplit(x, split = ".csv")[[1]][1]
    })
  )
  available <- rivers_needed %in% rivers_available

  if(sum(!available) > 0){
    cat("No river course available for:", paste0("\n", rivers_needed[!available]))
  }

  load_file_number <-  which(rivers_available %in% rivers_needed)

  river_files <- files[load_file_number]
  river_names <- rivers_available[load_file_number]

  rivers <- lapply(river_files, function(file){
    river <- read.table(file = file, header = T, sep = ";", dec = ".")
    river$qsim_site <- ""
    river
  })
  names(rivers) <- river_names

  for(i in 1:nrow(aggregated_data)){
    verknet_river <- aggregated_data$verknet_river[i]
    qsim_km <- aggregated_data$km[i]
    if(verknet_river %in% names(rivers)){
    site_distance <- abs(rivers[[verknet_river]]$km - qsim_km)
    nearest_location<- which(site_distance == min(site_distance))
    rivers[[verknet_river]]$qsim_site[nearest_location] <-
      paste(aggregated_data$river[i],
            aggregated_data$section[i],
            aggregated_data$km[i],
            sep = "_")
    }
  }

  rivers
}

#' Changes Qsim Ids to Verknet IDs
#'
#'
#' @param aggregated_data A dataframe created by one of [deviating_hours()],
#' [adverse_deviation_from_reference()] or [critical_events()]
#' @param translation_table Data frame of at least two columns: column number 1:
#' Verknet ID, column with name "sim_ID": Qsim ID
#'
#' @export
#'
qsim_to_verknet_id <- function(aggregated_data, translation_table){
  qsim_ids <- unlist(strsplit(x = translation_table$sim_ID, ","))
  aggregated_data$verknet_river <- aggregated_data$river
  for(qsim_id in qsim_ids){
    df_row <- grep(pattern = qsim_id, x = translation_table$sim_ID)
    verknet_id <- translation_table[[1]][df_row]
    aggregated_data$verknet_river <-
      gsub(
        pattern = qsim_id,
        replacement = verknet_id,
        x = aggregated_data$verknet_river
      )
  }
  aggregated_data
}
