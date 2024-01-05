#' Relative adverse deviation from a reference
#'
#' The cumulative sum of all adverse deviations.
#'
#' @param dataFrame Data frame with time in first column and parameter
#' values per site. If no reference data frame is provided it must include
#' the reference_vector as column.
#' @param reference Column name of the reference in dataFrame or a seperate
#' data frame that contains the same column names as dataFrame
#' @param worst A numerical value which is the worst case
#' @param good_values Character, either "high" or "low" defining what would be
#' best
#'
#' @details
#' First the similarity the data vector and the reference vector is calculated.
#' Only complete pairs (no NA values) are used. For each data pair the
#' quotient between data and reference is calcutaled. If data > reference the
#' value is set to 1. All quotients  are cumulated (-> absolute similarity).
#' This can be maximum the number of data pairs. When deviding by the number of
#' data pairs, the relative similarity is obtained. One minus the relative
#' similarity is the negative deviation.
#'
#' @return
#' Numeric value between 0 and 1
#'
#' @export
#'
adverse_deviation_from_reference <- function(
    dataFrame,reference, worst = 0, good_values = "high"
){
  dates <- dataFrame[,1]
  total_hours <- as.numeric(difftime(
    time1 = max(dates),
    time2 = min(dates),
    units = "hours"
  ))
  resolution <- as.numeric(difftime(
    time1 = dates[2],
    time2 = dates[1],
    units = "hours"
  ))
  t_step <- resolution / total_hours

  d <- dataFrame[,-1]
  sites <- colnames(d)

  if(is.character(reference)){
    reference_vector <- dataFrame[,reference]

    v <- apply(d, 2, function(data_vector){
      if(good_values == "high"){
        deviation_amount <- (data_vector - worst) / (reference_vector - worst)
        # Adverse if data is lower than reference and reference is higher than worst
        adverse_deviation <-
          data_vector < reference_vector & reference_vector > worst
      } else if(good_values == "low"){
        deviation_amount <-  1 - (data_vector - worst) / (reference_vector - worst)
        # Adverse if data is higher than reference and reference is lower than worst
        adverse_deviation <-
          data_vector > reference_vector & reference_vector < worst
      }
      round(sum(deviation_amount[adverse_deviation]) * t_step, digits = 3)
    })

  } else {
    v <- apply(sites, 2, function(site){
      data_vector <- d[,site]
      reference_vector <- reference[,site]
      if(good_values == "high"){
        deviation_amount <- (data_vector - worst) / (reference_vector - worst)
        # Adverse if data is lower than reference and reference is higher than worst
        adverse_deviation <-
          data_vector < reference_vector & reference_vector > worst
      } else if(good_values == "low"){
        deviation_amount <-  1 - (data_vector - worst) / (reference_vector - worst)
        # Adverse if data is higher than reference and reference is lower than worst
        adverse_deviation <-
          data_vector > reference_vector & reference_vector < worst
      }
      round(sum(deviation_amount[adverse_deviation]) * t_step, digits = 3)
    })
  }
  data.frame("adverse_dev" = v)
}
