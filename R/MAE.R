#' Calculate the mean absolute error for a data set
#' 
#' \code{MAE} calculates the mean of the absolute difference between two values
#' (predicted and actual), or the MAE. Any observations or predicted values that
#' are NA are removed.
#' @param y the actual (observed) value
#' @param ymod the modelled (predicted) value
#' @return a single value of the MAE
#' @export
#' 
#' @seealso \code{\link{GetErrorMetrics}}

MAE <- function(y,
                ymod){
  mean(abs(y - ymod),na.rm = TRUE)
}