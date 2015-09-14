#' Calculate a range of error metrics for a data set
#' 
#' \code{GetErrorMetrics} calculates a range of error metrics.
#' 
#' @param y the actual (observed) value
#' @param ymod the modelled (predicted) value
#' @return metrics a data.frame with columns for the metric and value
#' @export
#'   
#' @seealso \code{\link{PearsonCorrelation}, \link{RMSE}, \link{MAE},
#'   \link{MaxAE}, \link{SAE}}

GetErrorMetrics <- function(y,
                            ymod){
  x = cbind(y,ymod)
  funs <- c(PearsonCorrelation,
            RMSE,
            MaxAE,
            MAE,
            SAE,
            SPE)
  funnames <- c("Pearson Correlation",
                "RMSE",
                "Max(AE)",
                "MAE",
                "SAE",
                "SPE")
  value = sapply(funs, 
                 function(f) f(y = x[,1],
                               ymod = x[,2]))
  metrics <- data.frame(metric = funnames,
                type = "Error metric",
                value = as.matrix(value))
  return(metrics) 
}