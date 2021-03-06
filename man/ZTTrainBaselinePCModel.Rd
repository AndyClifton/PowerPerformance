% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/ZTTtrainBaselinePCModel.R
\name{ZTTrainBaselinePCModel}
\alias{ZTTrainBaselinePCModel}
\title{Create a power curve for a specific Ti level using the zero turbulence method}
\usage{
ZTTrainBaselinePCModel(ws, power, ti, ws.cutin = 2)
}
\arguments{
\item{ws}{observations of wind speed}

\item{power}{observations of power}

\item{PC.param}{a set of parameters describing the power curve}

\item{Ti}{observations of turbulence intensity}

\item{rho}{}

\item{NewTi}{the new value of turbulence intensity}
}
\value{
site specific power curve
}
\description{
\code{ZTTrainBaselinePCModel} creates a basic model of turbine power performance. The IEC method of binning to create.
}
\details{
The input to this function is generated by
\code{\link{ZTTrainFinalTurbineModel}}.
}
\seealso{
Other zero-turbulence power curve methods: \code{\link{SimPC}};
  \code{\link{ZTQueryTurbineModel}};
  \code{\link{ZTTrainFinalTurbineModel}};
  \code{\link{ZTTrainInitTurbineModel}};
  \code{\link{ZTTrainTheoTurbineModel}};
  \code{\link{ZTTrainTiSpecificTurbineModel}}
}

