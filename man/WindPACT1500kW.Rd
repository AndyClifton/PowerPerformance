% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/WindPACT1500kW.R
\docType{data}
\name{WindPACT1500kW}
\alias{WindPACT1500kW}
\alias{Windpact1500kW}
\alias{windpact1500kw}
\title{Simulated performance data for the WindPACT 1.5 MW Baseline wind turbine}
\description{
This data set is a combination of inflow wind and turbine power data for the
WindPACT 1,500 kW wind turbine. The data are based on simulations of the
turbine response to a turbulent inflow. The inflow was simulated using the
stochastic windfield modeling tool, "Turbsim". The turbine response was
simulated using the aero-elastic simulator "FAST". Data were post processed
using matlab to extract the variables described below.
}
\section{Data format}{
 A data frame with 1,524 observations of the following 7
  variables: \describe{\item{\code{ws.HH}}{hub-height wind speed (m/s)}
  \item{\code{Ti.HH}}{Turbulence intensity at hub height (\%)}
  \item{\code{Shear}}{The exponent of the power law fit to the velocity
  profile} \item{\code{ws.eq}}{A rotor-equivalent wind speed (m/s)}
  \item{\code{RSS}}{A metric describing the difference between the meaasured
  velocity profile and an ideal power-law profile}
  \item{\code{power.mean}}{The mean power under these conditions (kW)}
  \item{\code{power.std}}{The standard deviation of power under these
  conditions (kW)}}
}

\section{Turbine Characteristics}{
 The WindPACT 1.5 MW Baseline turbine has
  the following characteristics: \describe{ \item{Rotor diameter}{70 m}
  \item{Hub height}{84 m} \item{Rated power}{1500 kW} \item{Cut-in wind
  speed}{3 m/s}\item{Rated wind speed}{11.5 m/s} \item{Cut-out wind
  speed}{27.6 m/s}}
}
\seealso{
Other timeseries turbine data: \code{\link{PCWGDataSet1}},
  \code{\link{PCWGdataset1}}
}

