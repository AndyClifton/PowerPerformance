#' Calculate the sum of the absolute error for a data set
#' 
#' \code{SAE} calculates the sum of the absolute difference between two values
#' (predicted and actual), or the SAE. Any observations or predicted values that
#' are NA are removed.
#' @param y the actual (observed) value
#' @param ymod the modelled (predicted) value
#' @return a single value of the SAE
#' @export
#' 
#' @seealso \code{\link{GetErrorMetrics}}

SAE <- function(y,
                ymod){
  sum(abs(y - ymod),na.rm = TRUE)
}