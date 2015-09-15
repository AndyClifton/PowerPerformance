#' PowerPerformance: analyzing and predicting wind turbine performance
#' 
#' \code{PowerPerformance} provides tools for analyzing wind resource data, analyzing
#' wind turbine performance data, and predicting turbine performance. More
#' information about each function can be found in its help documentation.
#' @docType package
#' @name PowerPerformance
#' @aliases PowerPerformance PowerPerformance-package
#' @section Wind resource assessment: Functions to help in the analysis of wind
#'   resources: \itemize{\item \code{\link{GetREWS}} Calculates
#'   the rotor-equivalent wind speed from a vertical wind profile.}
#' @section Utility functions: Most of the following are utility functions that
#'   are called from workhorse functions included in \code{PowerPerformance}:
#'   \itemize{\item \code{\link{GetErrorMetrics}} summarizes the eror metrics
#'   between observed and predicted values}
#' @references Clifton, A, and Rozenn Wagner. 2014. “Accounting for 
#' the effect of turbulence on wind turbine power curves.” Journal of Physics: 
#' Conference Series 524 (1): 012109. 
#' \url{http://stacks.iop.org/1742-6596/524/i=1/a=012109} 
NULL