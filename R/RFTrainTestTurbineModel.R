#' Train and test a random forest power model
#' 
#' \code{RFTrainTestTurbineModel} trains and then tests a random forest power 
#' model of a wind turbine using actual observations. The function returns basic
#' error and CPU usage metrics. This function is intended for assessing 
#' the accuracy of a particular model.
#' @param train.predictors a data.frame containing the predictors that will be 
#'   used to train the model
#' @param train.response a vector containing the response of the turbine (this 
#'   could be power, loads, ...)
#' @param test.predictors a data.frame containing a new set of predictors that 
#'   will be use to create a new response matrix
#' @param test.response a vector of observed responses
#' @return a list containing \describe{\item{model}{the random forest model}
#'   \item{metrics}{a data.frame containing different error and performance
#'   metrics}}
#' @export
#'   
#' @seealso \code{\link{GetErrorMetrics}}
#' @family Random forest methods

RFTrainTestTurbineModel <- function(train.predictors,
                                    train.response,
                                    test.predictors,
                                    test.response){
  # train the model
  train.start <- proc.time()["elapsed"][[1]]
  model.RF <- RFTrainTurbineModel(predictors = train.predictors,
                                  response = train.response)  
  train.stop <- proc.time()["elapsed"][[1]]
  
  # get the importance of the variables we passed in (not needed in timin)
  importance.metrics <- data.frame(metric = rownames(importance(model.RF)),
                                   type = "Importance metric",
                                   value = unlist(importance(model.RF)[,1],
                                                  use.names = FALSE),
                                   row.names = NULL)
  
  # now we test the model
  test.start <- proc.time()["elapsed"][[1]]
  error.metrics <- GetErrorMetrics(y = test.response,
                                   ymod = RFQueryTurbineModel(predictors = test.predictors,
                                                              model = model.RF)$mean)
  test.stop <- proc.time()["elapsed"][[1]]
  # create a performance metrics data frame that is similar to the error.metrics  
  performance.metrics <- rbind(data.frame(metric = "Training time",
                                          type = "Performance metric",
                                          value = train.stop - train.start),
                               data.frame(metric = "Testing time",
                                          type = "Performance metric",
                                          value = test.stop - test.start))
  return(list(model = model.RF,
              metrics = rbind(error.metrics,
                              performance.metrics,
                              importance.metrics)))
}