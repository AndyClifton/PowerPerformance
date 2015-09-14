#' Check the importance of the variables used in the Random Forest model
#' 
#' \code{RFCheckVarImp} shows the importance of the different variables that
#' were input to the random forest model.
#' @param model.RF the trained random forest model
#' @export
#' 
#' @family Random forest methods

RFCheckVarImp <- function(model.RF){
# find out how important each variable is
ImpData=data.frame(importance(model.RF))
ImpData$ID <- row.names(ImpData)
# plot a varImpPlot(model.RF)
p.rf.importance <- ggplot(data=ImpData,
                          aes(x = ID,
                              y = X.IncMSE)) + 
  geom_bar(position="dodge",stat="identity") + 
  labs(x = "Data") +
  labs(y = "Importance") + 
  labs(title = "Contribution of data to random forest model")
print(p.rf.importance)
}