#' Train and test a power curve model
#' 
#' \code{PCTrainTest} trains and then tests a power curve model of a 
#' wind turbine using actual observations. The function returns basic error and 
#' performance (CPU) metrics. This function is intended for assessing the 
#' accuracy of a particular model.
#' @param train.predictors a data.frame containing the predictors that will be 
#'   used to train the model
#' @param train.response a vector containing the response of the turbine (this 
#'   could be power, loads, ...)
#' @param test.predictors a data.frame containing a new set of predictors that 
#'   will be use to create a new response matrix
#' @param test.response a vector of observed responses
#' @return a list containing \describe{\item{model}{the power curve} 
#'   \item{metrics}{a data.frame containing different error and performance 
#'   metrics}}
#' @export
#' @family Power curve methods
#' 
PCTrainTest <- function(train.predictors,
                                train.response,
                                test.predictors,
                                test.response){
  # train the power curve
  train.start <- proc.time()["elapsed"][[1]]
  # check to see if the turbulence intensity was passed in as a variable  
  if(("Ti" %in% colnames(train.predictors))==FALSE){
    # add it in
    train.predictors$Ti = NA
  }
  model.PC <- PCTrainTurbineModel(ws = train.predictors$ws,
                            power = train.response,
                            ti = train.predictors$Ti)
  train.stop <- proc.time()["elapsed"][[1]]
  
  # now test
  test.start <- proc.time()["elapsed"][[1]]
  error.metrics <- GetErrorMetrics(y = test.response,
                                   ymod = PCQueryTurbineModel(power.curve = model.PC,
                                                          ws = test.predictors$ws))
  test.stop <- proc.time()["elapsed"][[1]]
  # create a performance metrics data frame that is similar to the error.metrics  
  performance.metrics <- rbind(data.frame(metric = "Training time",
                                          type = "Performance metric",
                                          value = train.stop - train.start),
                               data.frame(metric = "Testing time",
                                          type = "Performance metric",
                                          value = test.stop - test.start))
  return(list(model = model.PC,
              metrics = rbind(error.metrics,
                              performance.metrics)))
}