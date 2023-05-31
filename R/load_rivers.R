#' Reads river data
#'
#' Reads all csv files starting with "river" in a folder and combines it in a
#' list. Each csv defines a river stretch in the region of interest and must at
#' least contain 5 columns:
#' x: longitude
#' y: lattitude
#' distance_to_neighbour: distance to the previous point of the river in km
#' km: distance from the river estuary
#' qsim_id: The qsim ID as d given by function [QSIM_prepare()] if the point
#' matches a qsim output site
#'
#' @param path Pathname with all river data
#'
#' @export
#'
load_rivers <- function(path){
  files <- dir(path, full.names = TRUE)
  filenames <- dir(path, full.names = FALSE)

  rivers <- grep(pattern = "^river_", x = filenames)
  river_files <- files[rivers]
  river_names <- filenames[rivers]
  river_names <- substr(x =  river_names,
                        start = 7, stop = nchar(river_names) - 4)

  rivers <- lapply(river_files, function(file){
    river <- read.table(file = file, header = T, sep = ";", dec = ".")
  })

  names(rivers) <- river_names
  rivers
}
