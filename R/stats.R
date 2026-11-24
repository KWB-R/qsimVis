#' General statistics per site
#'
#' @param dataFrame Data frame with time column "posixDateTime" and parameter
#' values per site. NA values are not removed, make sure there are no NA values
#' in the time series.
#'
#' @return Data frame with rows per site and columns for minimum, median,
#' maximum, the 25th, 75th and 90th Percentile, mean and standard deviation
#'
#' @importFrom stats quantile sd
#' @export
#'
stats <- function(
    dataFrame,
    quantiles = c(0.25, 0.5, 0.75)
){
  d <- dataFrame[,-grep(pattern = "posixDateTime", x = colnames(dataFrame)),
                 drop = FALSE]
  df_out <- t(sapply(d, function(x){
    v_out <- c(
      quantile(x = x, probs = c(0, quantiles, 1)),
      mean(x),
      sd(x))
    names(v_out) <- c("min", paste0("q", quantiles*100), "max", "mean", "sd")
    v_out
  }))
  df_out <- qsimVis::add_site_info(
    df_in = df_out,
    v_qsim_ids = rownames(df_out)
  )
  df_out
}


