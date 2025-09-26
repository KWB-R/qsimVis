#' Reads river data
#'
#' Reads all required verknet river csv files, optionally complemented by
#' manually added river files. Each csv defines a river stretch and must contain
#' 4 columns:
#' x: longitude
#' y: lattitude
#' km: distance from the river estuary
#' distance_to_neighbour: distance to the previous point of the river in km
#'
#' @param qsimVis_IDs A character vector of qsimVis IDs as listed in the
#' translation table
#' @param path_manual A path to manually added river tables. If NULL (default)
#' no manually added rivers are assumed.
#'
#' @export
#'
load_rivers <- function(qsimVis_IDs, path_manual = NULL){

  path_verknet <- system.file(package = "qsimVis", "extdata/verknet")
  verknet_rivers <- available_rivers(path = path_verknet)
  manual_rivers <- if(!is.null(path_manual)){
    available_rivers(path = path_manual)
  } else {
    NULL
  }

  rivers_needed <- unique(qsimVis_IDs)
  available <- rivers_needed %in% c(verknet_rivers, manual_rivers)

  if(sum(!available) > 0){
    cat("No river course available for:", paste0("\n", rivers_needed[!available]))
  }

  verknet_files <- verknet_rivers[verknet_rivers %in% rivers_needed]
  rivers <- lapply(verknet_files, function(file){
    read.table(
      file = file.path(path_verknet, paste0(file, ".csv")),
      header = T,
      sep = ";",
      dec = "."
    )

  })
  names(rivers) <- verknet_files

  if(!is.null(path_manual)){
    # all manually added rivers are plotted
    n_verknet <- length(rivers)
    mr <- manual_rivers[manual_rivers %in% rivers_needed]
    rivers <- c(
      rivers,
      lapply(mr, function(file){
        read.table(
          file = file.path(path_manual, paste0(file, ".csv")),
          header = T,
          sep = ";",
          dec = "."
        )
      })
    )
    names(rivers)[-(1:n_verknet)] <- mr
  }
  rivers
}
