#' General statistics per site
#'
#' @param dataFrame Data frame with time column "posixDateTime" and parameter
#' values per site. NA values are not removed, make sure there are no NA values
#' in the time series.
#'
#' @return Data frame with rows per site and columns for minimum, median,
#' mean, standard diviation and maximum
#'
#' @importFrom stats quantile sd
#' @export
#'
stats <- function(
    dataFrame
){
  d <- dataFrame[,-grep(pattern = "posixDateTime", x = colnames(dataFrame))]
  df_out <- t(sapply(d, function(x){
    v_out <- c(
      quantile(x = x),
      mean(x),
      sd(x))
    names(v_out) <- c("min", "q25", "median", "q75", "max", "mean", "sd")
    v_out
  }))
  df_out <- qsimVis::add_site_info(
    df_in = df_out,
    v_qsim_ids = rownames(df_out)
  )
  df_out
}


