#' Calculate the maximum absolute error for a data set
#' 
#' \code{MaxAE} calculates the mean of the absolute difference between two values
#' (predicted and actual), or the MAE. Any observations or predicted values that
#' are NA are removed.
#' @param y the actual (observed) value
#' @param ymod the modelled (predicted) value
#' @return a single value of the MAE
#' @export
#' 
#' @seealso \code{\link{GetErrorMetrics}}

MaxAE <- function(y,
                ymod){
  max(abs(y - ymod),na.rm = TRUE)
}