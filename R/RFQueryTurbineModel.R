#' Use a random forest model to predict response to forcing conditions
#' 
#' \code{RFQueryTurbineModel} uses a trained random forest model to estimate the
#' response to a set of forcing conditions (predictors).
#' @param predictors a data.frame of predictors corresponding to the data.frame
#'   used to train the model
#' @param model a model object that was trained using predictors with the same names
#' @return mean and standard deviation of the values from the model
#' @export
#'   
#' @family Random forest methods

RFQueryTurbineModel <- function(predictors,
                                model){
  
  rf <- predict(object = model,
                newdata = predictors,
                type = "response",
                predict.all = TRUE)
  # get the data
  return(data.frame(mean = apply(rf$individual, 1, mean),
                    sdev = apply(rf$individual, 1, sd)))
}