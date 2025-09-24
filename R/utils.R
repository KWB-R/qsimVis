#' Calculates x- and y-scale based on longitude and lattitude data
#'
#' Get the right scale for plotting a map. While width must be defined,
#' the according height is calculated
#'
#' @param xlim Minimum and maximum longitude of the map
#' @param ylim Minimum and maximum lattitude of the map
#' @param width Numeric value defining the width of the plot (in inch)
#'
#' @details
#' The ratio between width and height is directly based on the ratio between
#' lonigtude and lattitude. In order to get the correct scale for the plot the
#' margins need to be set to 0 (see example).
#'
#' @return
#' Named vector of two, containing the width (for x-axis dimension) and the
#' height (for y-axis dimension).
#'
#' @export
#' @importFrom geosphere distHaversine
#'
#' @examples
#' xlim <- c(13.18, 13.472)
#' ylim <- c(52.46, 52.57)
#' plotDim <- getDimensions(xlim = xlim, ylim = ylim, width = 10)
#' plotDim
#'
#' dev.new(noRStudioGD = TRUE, width = plotDim[1], height = plotDim[2])
#' par(mar = c(0, 0, 0, 0)) # no margins outside of the plot
#' plot(x = xlim, y = ylim, type = "l")
#' text(x = mean(xlim), y = ylim[2],
#'      labels = "Correctly Scaled Line",
#'      pos = 1, cex = 3)
#'
getDimensions <- function(xlim, ylim, width = 10){
  x_dist <- geosphere::distHaversine(
    p1 = c(xlim[1], ylim[1]),
    p2 = c(xlim[2], ylim[1])) / 1000 # in km

  y_dist <- geosphere::distHaversine(
    p1 = c(xlim[2], ylim[1]),
    p2 = c(xlim[2], ylim[2])) / 1000 # in km

  c("width" = width, "height" = y_dist / x_dist * width)
}

#' Repeating values in a row within a Vector
#'
#' Describes the Values of a vector the times they are repeated and the
#' start and end position of those values
#'
#' @param v A character, factor or numeric vector
#'
#' @details
#' Parts of this function originally come from package kwb.utils::findChanges()
#'
#' @return
#' A data frame with four columns: Value (-> listed value of the input vector),
#' Repeats (times it is repeated in a row), starts_at (start position),
#' ends_at (end position).
#'
same_inarow <- function(v){

  stopifnot(!anyNA(v))

  n <- length(v)
  if (n == 0L) {
    return(NULL)
  }
  changes_at <- which(v[-n] != v[-1L]) + 1L
  result <- data.frame(
    "starts_at" = c(1L, changes_at),
    "ends_at" = c(changes_at -  1L, n),
    "value" = v[c(1L, changes_at)],
    stringsAsFactors = FALSE
  )

  data.frame(
    Value = result$value,
    repeats = result$ends_at - result$starts_at + 1L,
    starts_at = result$starts_at,
    ends_at = result$ends_at
  )
}

#' Lists all filenames of csv files in a folder
#'
#' @param path The file path
#'
#'
available_rivers <- function(path){
  files <- dir(path, full.names = TRUE)
  filenames <- dir(path, full.names = FALSE)
  unique(
    sapply(filenames, function(x){
      strsplit(x, split = ".csv")[[1]][1]
    })
  )
}

#' Takes a date as character and returns the most likely format if possible
#'
#' @param chracterDates A vector of dates as character
#' @param return_positions If TRUE, a dataframe with start and stop positions
#' of the part of the datetime is returned, which can be used to cut strings
#' at certain position. If FALSE (default) the most probably date format is
#' returned which can be used as format argument in POSIX functions
#' @param check_format If TRUE, all parts of the timestampt are checked against
#' their constraints (ex. days must have values between 1 and 31)
#'
#' @importFrom methods as is
#'
findDateFormat <- function(
    chracterDates, return_positions = FALSE, check_format = FALSE
){
  d <- chracterDates[1]
  t_seperator <- grepl(pattern = "T", x = d)
  if(t_seperator){
    d <- sub(pattern = "T", replacement = " ", x = d)
  }

  d_parts <- strsplit(x = d, split = "[[:punct:]]|[[:space:]]")[[1]]
  n_numbers <- nchar(d_parts)
  sep_i <- cumsum(n_numbers[-length(d_parts)] + 1)
  d_seps <- sapply(sep_i , function(s){
    substr(x = d, start = s, stop = s)
  })
  d_seps <- c(d_seps, "")

  am_pm <- if(any(c("pm", "am") %in% d_parts)){
    TRUE
  } else {
    FALSE
  }

  time_seps <- d_seps == ":"
  if(sum(time_seps) > 0L){
    time_sep_i <- which(time_seps)
    time_sep_i <- unique(c(time_sep_i, time_sep_i + 1))
    names(d_parts)[time_sep_i[1]] <- ifelse(am_pm, "I", "H")
    names(d_parts)[time_sep_i[2]] <- "M"
    if(length(time_sep_i) == 3L){
      names(d_parts)[time_sep_i[3]] <- "S"
    }
    if(am_pm){
      time_sep_i <- unique(c(time_sep_i, time_sep_i + 1))
      names(d_parts)[max(time_sep_i)] <- "p"
    }
    time_seps <- seq_along(d_parts) %in% time_sep_i
  }

  date_i <- seq_along(d_parts)[!time_seps]
  if(length(date_i) == 3L){
    year_i <- which(n_numbers[date_i] == 4L)
    if(length(year_i) == 1L){
      names(d_parts)[year_i] <- "Y"
      names(d_parts)[date_i[2]] <- "m"
      names(d_parts)[date_i[-c(year_i, 2)]] <- "d"
    }
  }
  date_parts <- data.frame("datePart" = names(d_parts),
             "start" = c(1, sep_i+1),
             "stop" = c(sep_i-1, nchar(d)))

  if(check_format){
    part_values <- list("d" = 1:31,
                        "m" = 1:12,
                        "H" = 0:23,
                        "I" = 1:12,
                        "M" = 0:60,
                        "S" = 0:60,
                        "p" = c("am", "pm"))

    date_parts$correct <- NA
    for(i in 1:nrow(date_parts)){
      part_name <- date_parts$datePart[i]
      if(part_name %in% names(part_values)){
        value_type <- is(part_values[[part_name]])[1]
        all_values <- as(unique(
          substr(x = chracterDates,
                 start = date_parts$start[i],
                 stop = date_parts$stop[i])
        ), Class = value_type)
        date_parts$correct[i] <- all(all_values %in% part_values[[part_name]])
      }
    }
    if(sum(!date_parts$correct, na.rm = TRUE) > 0L){
      stop("The identified format does not meet the constraints of all timestamp parts.")
    } else {
      print("Format was checked and is probably correct. No deviations found.")
    }
  }

  if(return_positions){
    date_parts
  } else {
    if(t_seperator){
      d_seps[min(time_sep_i) - 1] <- "T"
    }
    paste0("%", names(d_parts), d_seps, collapse = "")
  }
}


#' Returns the days of time shift for all years part of a date vector
#'
#' @param all_dates Date-Time vector as character
#'
#' @return
#' A list of dates of timeshifts per year
#'
daysOfTimeShift <- function(all_dates){

  date_parts <- findDateFormat(chracterDates = all_dates,
                              return_positions = TRUE)

  y <- date_parts$datePart == "y"
  all_years <- unique(
    substr(x = all_dates, start = date_parts$start[y], stop = date_parts$stop[y])
  )

  lapply(all_years, function(x){
    year_i <- as.numeric(x)
    whole_year <- seq(
      from = as.Date(paste0(year_i - 1, "-12-31")),
      to = as.Date(paste0(year_i + 1, "-01-01")),
      by = 1
    )

    os <- format(as.POSIXct(whole_year,  tz = "Europe/Berlin"), "%z")
    os_changes <- c(
      which(!duplicated(os, fromLast = FALSE)[-1]),
      which(!duplicated(os, fromLast = TRUE)[-length(os)])
    )

    whole_year[os_changes]
  })
}


