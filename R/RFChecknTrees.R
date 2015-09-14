#' Check if sufficient trees were used in the random tree model
#' 
#' \code{RFChecknTrees} shows the output from a random forest model. The RMSE is
#' plot versus the number of trees. This metric should be flat if enough trees
#' were used.
#' @param model.RF the trained random forest model
#' @export
#' 
#' @family Random forest methods

RFChecknTrees <- function(model.RF){  
  # look at how the model improves with number of trees
  p.rf.ntrees <- ggplot(data = data.frame('trees' = seq(1, model.RF$ntree, by = 1),
                                          'mse' = model.RF$mse),
                        aes(x = trees,
                            y = mse)) +
    geom_line() +
    labs(x = "trees") +
    labs(y = "Mean Square Error") + 
    labs(title = "Accuracy versus number of trees")
  print(p.rf.ntrees)    
  return(p.rf.ntrees)
}