#' Count Events of deficits
#'
#' Counts the Number of intervals where a minimum to time in a row are below
#' a defined threshold value. Events are separated by a specified period of
#' time above that threshold. Furthermore, the exceedance
#' of a value can also be a separation criterion.
#'
#' @param dataFrame Data frame with posixDateTime column and at leaste 2 site
#' columns
#' @param deficiency_hours Minimal time period of values below threshold.
#' @param separating_hours Minimal time period between two values below
#' threshold to count as separate events.
#' @param threshold Numeric in the same unit as the data vector
#' @param dev_type Either "elt" (equal or lower than) or "egt" (equal or higher
#' than) to find events below or above the threshold value.
#' @param recovery_value If this is not NULL (default) two events are only
#' separated if a numerical revocvery value is exceeded between two deficits.
#' That does not impact the event seperation by time (seperating_hours).
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
    dev_type = "elt",
    recovery_value = NULL,
    return_event_positions = FALSE
){
  dates <- dataFrame[["posixDateTime"]]
  resolution <- as.numeric(difftime(
    time1 = dates[2],
    time2 = dates[1],
    units = "hours"))

  starting_data_points <- round(1 + deficiency_hours / resolution, 0)
  following_data_points <- starting_data_points - 1
  separating_data_points <- round(separating_hours / resolution, 0)


  d <- dataFrame[,-grep(pattern = "posixDateTime", x = colnames(dataFrame))]
  v <- apply(d, 2, function(data_vector){

    if(dev_type == "elt"){
      possible_starts <- which(data_vector <= threshold)

      # criteria 1: the following values, depending on deficiency_hours, need to
      # be below threshold, too
      following <- sapply(1:following_data_points, function(i){
        (data_vector[possible_starts + i] <= threshold)
      })
    } else if(dev_type == "egt"){
      possible_starts <- which(data_vector >= threshold)

      # criteria 1: the following values, depending on deficiency_hours, need to
      # be below threshold, too
      following <- sapply(1:following_data_points, function(i){
        (data_vector[possible_starts + i] >= threshold)
      })
    }

    event_starts <- if(length(unlist(following)) > 0L){
      if(sum(unlist(following), na.rm = T) > 0L){
        possible_starts[apply(following, MARGIN = 1, all)]
      }
    }
    events <- sort(unique(c(event_starts, event_starts + following_data_points)))

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
            recovering <- if(dev_type == "elt"){
              data_vector[a$tEnd[r - 1]:a$tBeg[r]] >= recovery_value
            } else if(dev_type == "egt"){
              data_vector[a$tEnd[r - 1]:a$tBeg[r]] <= recovery_value
            }
            recovered <- sum(recovering,na.rm = TRUE) > 0L
            separated <- c(separated,recovered)
          }
        }
        a$start <- separated
        a$end <- FALSE
        a$end[c((which(a$start) - 1)[2:sum(a$start)], nrow(a))] <- TRUE

        if(return_event_positions){
          data.frame(lapply(
            a[a$end == TRUE, c("tBeg", "tEnd")],
            function(x){
              dates[x]
            }))
        }  else {
          sum(a$end)
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
