laggedCorrelation <- function(x,
                              y,
                              lags = seq(-24,24,1),
                              lagunit = "hour"){
  lagged.cor <- NULL  
  # add the lag to y to figure out the data we want
  if(lagunit == "%H" |
       lagunit == "Hours" | lagunit == "hours" |
       lagunit == "Hour" | lagunit == "hour"){
    n.seconds = 60 * 60
  } 
  if((lagunit == "%M" |
        lagunit == "Minutes" | lagunit == "minutes" |
        lagunit == "Minute" | lagunit == "minute")){
    n.seconds  = 60
  } 
  if((lagunit == "%S" |
        lagunit == "Seconds" | lagunit == "seconds" |
        lagunit == "Second" | lagunit == "second")) {
    n.seconds = 1
  }
  for (lag in lags){
    dt = lag * n.seconds
    y$timestamp.lagged = as.numeric(format(y$timestamp, "%s")) + dt
    # find data in x and y that have the same timestamp
    merged <- merge(x, y[c("timestamp.lagged","data")],
                    by.x = "timestamp",
                    by.y = "timestamp.lagged")
    
    # get the correlation        
    lagged.cor <- rbind(lagged.cor,
                       data.frame(lag = lag,
                                  cor = cor(merged$data.x, 
                                            merged$data.y),
                                  grad = tryCatch(lm(merged$data.y ~ merged$data.x)$coeff[2],
                                                  error = function(cond){return(NA)},
                                                  warning = function(cond){return(NA)})))
  }  
  
  return(lagged.cor)
}