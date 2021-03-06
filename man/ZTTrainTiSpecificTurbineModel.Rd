% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/ZTTrainTiSpecificTurbineModel.R
\name{ZTTrainTiSpecificTurbineModel}
\alias{ZTTrainTiSpecificTurbineModel}
\title{Create a power curve for a specific Ti level using the zero turbulence method}
\usage{
ZTTrainTiSpecificTurbineModel(PC.param, ws, Ti, power, rho = 1.225, newTi)
}
\arguments{
\item{PC.param}{a set of parameters describing the power curve}

\item{ws}{observations of wind speed}

\item{Ti}{observations of turbulence intensity}

\item{power}{observations of power}

\item{rho}{}

\item{NewTi}{the new value of turbulence intensity}
}
\value{
ZTTiSpecificTurbineModel site specific power curve
}
\description{
\code{ZTTrainTiSpecificTurbineModel} calculates the site-specific power curve using the
zero-turbulence method set out in the draft version of IEC 61400-12-1 (2013).
This should be the fourth (and final) stage in calculating a site-specific
power curve using the zero-turbulence power curve methdology.
}
\details{
The input to this function is generated by
\code{\link{ZTTrainFinalTurbineModel}}.
}
\seealso{
Other zero-turbulence power curve methods: \code{\link{SimPC}};
  \code{\link{ZTQueryTurbineModel}};
  \code{\link{ZTTrainBaselinePCModel}};
  \code{\link{ZTTrainFinalTurbineModel}};
  \code{\link{ZTTrainInitTurbineModel}};
  \code{\link{ZTTrainTheoTurbineModel}}
}

