#' Calculate the root mean square error for a data set
#' 
#' \code{RMSE} calculates the root of the mean of the square of the difference
#' between two values (predicted and actual), or the RMSE. Any values that are 
#' NA are removed.
#' @param y the actual (observed) value
#' @param ymod the modelled (predicted) value
#' @return a single value of the RMSE
#' @export
#' 
#' @seealso \code{\link{GetErrorMetrics}}

RMSE <- function(y,
                 ymod){
  sqrt(mean((y - ymod)^2,na.rm = TRUE))
}