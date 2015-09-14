#' Train a random forest model
#' 
#' \code{RFTrainTurbineModel} trains a random forest model. Althrough the
#' package assumes that the model will be power versus some forcing conditions,
#' the function can be use to train a model with any combination of predictor
#' and response data.
#' @param predictors a data.frame of predictive (forcing) values
#' @param response a vector of response data
#' @param ntree the number of trees to try out (default = 100)
#' @return model.RF the resulting model
#' @export
#'   
#' @family Random forest methods

# create the model from the data
RFTrainTurbineModel <- function(predictors,
                                response,
                                ntree = 1001,
                                ...){
  # set a random seed so that we have repeatable results  
  set.seed(1)
  # train & tune the random forest model using the training data set 
  model.RF <- randomForest(x = predictors,
                     y = response,
                     na.action = na.omit,
                     ntree = ntree,
                     mtry = 2,
                     replace = TRUE,
                     ...)
  # The key to a successful model is to increase mtry to at least 2.
  # the maximum value is the number of variables in the data set.
  # otherwise only 1 variable is tried at each node (Duh). 
  # The only problem with mtry = NROW(x) is that the model may be over-fitted.  
  return(model.RF)
  
}