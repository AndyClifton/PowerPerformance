#' Calculate the Pearson's Correlation Coefficient for a dataset
#' 
#' \code{PearsonCorrelation} calculates the Pearson's correlation coeffcient between two variables. Any observations or predicted values that
#' are NA are removed. This can be considered the sum of the underprediction
#' @param y the actual (observed) value
#' @param ymod the modelled (predicted) value
#' @return the Pearson Correlation Coefficient,. Use $statistic to access the value from the test.
#' @export
#' 
#' @seealso \code{\link{GetErrorMetrics}}

PearsonCorrelation <- function(y,
                               ymod){
  r <- cor(x = y, 
           y = ymod,
           method = "pearson")
  return(r)
}