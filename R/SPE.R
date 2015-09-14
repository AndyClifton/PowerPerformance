#' Calculate the sum of the positive error for a data set
#' 
#' \code{SPE} calculates the sum of the positive difference between two values
#' (actual - predicted), or the SPE. Any observations or predicted values that
#' are NA are removed. This can be considered the sum of the underprediction
#' @param y the actual (observed) value
#' @param ymod the modelled (predicted) value
#' @return a single value of the SPE
#' @export
#' 
#' @seealso \code{\link{GetErrorMetrics}}

SPE <- function(y,
                ymod){
  usei = ((y - ymod) > 0)
  sum((y[usei] - ymod[usei]),na.rm = TRUE)
}