#' Performance data set from a wind turbine (PCWG #1)
#' 
#' This data set contains hub-height wind speed and turbulence data for a
#' utility-scale wind turbine. Data were retrieved from the Power Curve Working
#' Group Dataset #1. Data can be found at \url{http://www.pcwg.org}
#' 
#' @name PCWGDataSet1
#' @docType data
#' @aliases PCWGdataset1
#' @section Data format: A data frame with 10,653 observations of the following 
#'   variables: 
#'   \describe{
#'   \item{\code{ws.HH}}{hub-height wind speed (m/s),
#'   originally 'Hub Wind Speed'} 
#'   \item{\code{Ti.HH}}{Turbulence intensity at
#'   hub height (\%), originally 'Hub Turbulence' }
#'   \item{\code{Shear}}{The exponent of the power law fit to the velocity profile, originally 'Shear
#'   Exponent'} 
#'   \item{\code{ws.eq}}{A rotor-equivalent wind speed (m/s),
#'   originally 'Rotor Equivalent Wind Speed'} 
#'   \item{\code{power.mean}}{The mean
#'   power under these conditions (kW), originally 'Hub Power'}} 
#'   
#' @section Turbine Characteristics: The PCWG Dataset 1 turbine has 
#'   the following characteristics: \describe{ \item{Rotor diameter}{xx m} 
#'   \item{Hub height}{x m} \item{Rated power}{xxxx kW} \item{Cut-in wind 
#'   speed}{x m/s}\item{Rated wind speed}{xx.x m/s} \item{Cut-out wind 
#'   speed}{xx.x m/s}}
#' @family timeseries turbine data
NULL
