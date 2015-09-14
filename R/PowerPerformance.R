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
#' @references Clifton Andrew, Kilcher Levi, Fleming Paul, Ludquist Julie Kay
#' (2013). Environmental Research Letters, 8(2), 024009.
#' \url{http://iopscience.iop.org/1748-9326/8/2/024009}.
NULL