PCscatter <- function(ws,
                      power,
                      power.curve){
  # trim the power curve to points that only have power 
  power.curve.use <- power.curve[power.curve$power.binmean > 0,]
  # add an zero at the low speed end of the power curve
  power.curve.use <- rbind(data.frame("ws.binmin" = min(power.curve.use$ws.binmean)-0.75,
                                      "ws.binmax" = min(power.curve.use$ws.binmean)-0.25,
                                      "ws.binnpoints" = 0,
                                      "ws.binmean" = min(power.curve.use$ws.binmean)-0.5,
                                      "ti.binmean" = NA,
                                      "power.binmean" = 0,
                                      "power.binsd" = NA),
                           power.curve.use)
  
  # work through each segment in the power curve
  seg.j = rep(NA,NROW(ws))
  for (i in 2:NROW(power.curve.use)){
    # find the data points in this region
    inseg = (ws >= power.curve.use$ws.binmean[i-1]) & (ws < power.curve.use$ws.binmean[i])
    # save the segment number
    seg.j[inseg] = i-1  
  }
  # get the bin that the points are in as well
  bin.j = rep(NA,NROW(ws))
  for (i in 1:NROW(power.curve.use)){
    # find the data points in this region
    inbin = (ws >= power.curve.use$ws.binmin[i]) & (ws < power.curve.use$ws.binmax[i])
    # save the bin number
    bin.j[inbin] = i  
  }
  
  # get the offset of each point
  dPdu = ((power.curve.use$power.binmean[seg.j+1] - 
               power.curve.use$power.binmean[seg.j]) /
              (power.curve.use$ws.binmean[seg.j+1] - 
                 power.curve.use$ws.binmean[seg.j]))
  Pws = power.curve.use$power.binmean[seg.j] + (ws - power.curve.use$ws.binmean[seg.j]) * dPdu
  power.diff = power - Pws
  # aggregate this to get the error in each bin
  error = aggregate(cbind(power.diff)~bin.j,
                    data = data.frame(power.diff,
                                      bin.j),
                    FUN = function(x) {
                      ((1/sum(!is.na(x)))*sum(x^2, na.rm = TRUE))^(1/2)
                    })
  return(cbind(power.curve.use[error$bin.j,],
               scatter = error$power.diff))
}