
#' Flow weighted mean per site
#'
#' @param dataFrame Data frame with time column "posixDateTime" and parameter
#' values per site. NA values are not removed, make sure there are no NA values
#' in the time series.
#' @param df_flow Data frame in the same format as dataFrame with flow values
#' per site.
#'
#' @return Data frame with rows per site and columns flow proportional mean
#'
#' @export
#'
flow_weighted_mean <- function(
    dataFrame, df_flow
){
  d <- dataFrame[,-grep(pattern = "posixDateTime", x = colnames(dataFrame))]
  df <- df_flow[,-grep(pattern = "posixDateTime", x = colnames(df_flow))]
  if(any(colnames(d) != colnames(df))){
    stop("Columns of the parameter dataframe and the flow dataframe are not the same")
  }

  df_out <- matrix(sapply(seq_len(ncol(d)), function(i){
    sum(d[,i] * df[,i]) / sum(df[,i])
  }), ncol = 1)

  colnames(df_out) <- "flow_mean"

  df_out <- qsimVis::add_site_info(
    df_in = df_out,
    v_qsim_ids = colnames(d)
  )
  df_out
}
