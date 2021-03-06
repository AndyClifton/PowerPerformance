% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/plotPC.R
\name{plotPC}
\alias{plotPC}
\title{Workhorse function to plot power curves using different types of input}
\usage{
plotPC(model, param, ws = seq(3, 30, 0.5))
}
\arguments{
\item{model}{a data.frame containing bin-averaged wind speeds and power. Must
include at least the columns \code{ws.binmean} and \code{power.binmean}.
The standard deviation of power in the bin can be passed in as
\code{power.binsd}}

\item{param}{a set of parameters that describe the power curve}
}
\value{
a ggplot object
}
\description{
\code{plotPC} uses \code{ggplot2} to plot power curves using either
bin-averaged data or parameters that describe the power curve.
}
\seealso{
Other Power curve methods: \code{\link{PCQueryTurbineModel}};
  \code{\link{PCTrainTest}};
  \code{\link{PCTrainTurbineModel}};
  \code{\link{PCwsAdjDens}}
}

