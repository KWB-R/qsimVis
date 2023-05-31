#' Count Events of deficits
#'
#' Counts the Number of intervals where a minimum to time in a row are below
#' a defined threshold value. Events are separated by a specified period of
#' time above that threshold. Furthermore, the exceedance
#' of a value can also be a separation criterion.
#'
#' @param dataFrame Data frame with time in first column and parameter
#' values per site.
#' @param deficiency_hours Minimal time period of values below threshold.
#' @param separating_hours Minimal time period between two values below
#' threshold to count as separate events.
#' @param threshold Numeric in the same unit as the data vector
#' @param recovery_value If this is not NULL (default) two events are only
#' separated if a numerical revocvery value is exceeded between two deficits.
#' @param return_event_positions Instead the number of events, the events
#' starting and end positions are returned, corresponding to the data vector
#'
#' @details
#' If deficiency_hours and seperating_hours lead to decimal number of data
#' points, the numerical is rounded to the next integer. For example:
#' a minimum deficiency time of 20 minutes in a 15 minute resolution would mean
#' at least 2.3 data points must be below threshold values which is rounded
#' to 2 data points, while a deficiency time of 25 minutes would result in 3
#' data points that needed to be below the threshold.
#'
#' @return Either a number of events or a data frame with event start and end
#' position
#'
#' @export
#'
critical_events <- function(
    dataFrame,
    separating_hours,
    deficiency_hours,
    threshold,
    recovery_value = NULL,
    return_event_positions = FALSE
){
  dates <- dataFrame[,1]
  resolution <- as.numeric(difftime(
    time1 = dates[2],
    time2 = dates[1],
    units = "hours"))

  starting_data_points <- round(1 + deficiency_hours / resolution, 0)
  separating_data_points <- round(separating_hours / resolution, 0)


  d <- dataFrame[,-1]
  v <- apply(d, 2, function(data_vector){
    possible_starts <- which(data_vector < threshold)

    # criteria 1: the following value needs to be below threshold, too
    following <- sapply(1:(starting_data_points - 1), function(i){
      (data_vector[possible_starts + i] < threshold)
    })

    events <- if(length(unlist(following))){
      if(sum(unlist(following), na.rm = T)){
        possible_starts[apply(following, MARGIN = 1, all)]
      }
    }


    if(length(events) > 0){
      # criteria 2: events within a defined seperation step are aggregated as one
      event_end <- c(which(diff(events) > separating_data_points))
      event_start <- c(1, event_end + 1)
      event_end <- c(event_end, length(events))
      a <- data.frame("tBeg" = events[event_start], "tEnd" = events[event_end])


      if(!is.null(recovery_value)){
        # possible criteria 3: minimum reached O2 concentration between events
        separated <- c(TRUE)
        if(nrow(a) > 1){
          for(r in 2:nrow(a)){
            separated <- c(
              separated,
              sum(
                data_vector[a$tEnd[r - 1]:a$tBeg[r]] >=
                  recovery_value,
                na.rm = TRUE) > 0)
          }
        }
        a$start <- separated
        a$end <- FALSE
        a$end[c((which(a$start) - 1)[2:sum(a$start)], nrow(a))] <- TRUE

        if(return_event_positions){
          data.frame(lapply(a, function(x){dates[x]}))
        }  else {
          sum(a$event)
        }
      } else {
        if(return_event_positions){
          data.frame(lapply(a, function(x){dates[x]}))
        } else {
          length(event_end)
        }
      }
    } else {
      if(return_event_positions){
        NULL
      } else {
        0
      }
    }
  })
  if(return_event_positions){
    do.call(rbind, v)
  } else {
    data.frame("events" = v)
  }
}
