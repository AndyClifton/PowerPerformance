% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/RFChecknTrees.R
\name{RFChecknTrees}
\alias{RFChecknTrees}
\title{Check if sufficient trees were used in the random tree model}
\usage{
RFChecknTrees(model.RF)
}
\arguments{
\item{model.RF}{the trained random forest model}
}
\description{
\code{RFChecknTrees} shows the output from a random forest model. The RMSE is
plot versus the number of trees. This metric should be flat if enough trees
were used.
}
\seealso{
Other Random forest methods: \code{\link{RFCheckVarImp}};
  \code{\link{RFQueryTurbineModel}};
  \code{\link{RFTrainTestTurbineModel}};
  \code{\link{RFTrainTurbineModel}}
}

