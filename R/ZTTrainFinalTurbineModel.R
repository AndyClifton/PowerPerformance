#' Create the final zero-turbulence power curve
#' 
#' \code{ZTTrainFinalTurbineModel} calculates the final zero-turbulence power curve
#' using the method set out in the draft version of IEC 61400-12-1 (2013). This
#' should be the third stage in calculating a site-specific power curve using
#' the zero-turbulence power curve methdology. 
#' 
#' The input to this function is generated by \code{\link{ZTTrainTheoTurbineModel}}.
#' 
#' The result of this function is used as the input to \code{\link{ZTSiteTurbineModel}}.
#' 
#' @param ws wind speeds
#' @param Ti turbulence intensity
#' @param power observed power
#' @param rho density
#' @return df.PC.zt the final binned zero-turbulence power curve
#' @export
#' @family zero-turbulence power curve methods
#' 
ZTTrainFinalTurbineModel <- function(PC.param,
                                     ws,
                                     Ti,
                                     power,
                                     rho = 1.225){
  
  # get the simulated power values at the real Ti
  PsimIobs <- SimPC(PC.param = PC.param,
                    ws = ws,
                    Ti = Ti,
                    rho = rho)
  # get the simulated power values at Ti = 0
  PsimIzero <- SimPC(PC.param = PC.param,
                     ws = ws,
                     Ti = 0.0,
                     rho = rho)
  
  # get the actual power at I ref = 0
  PIzero = power - PsimIobs + PsimIzero
  
  # now extend the power curve out to 100 m/s
  values = PCTrainTurbineModel(ws = ws,
                               power = PIzero)
  OK = (values$ws.binnpoints > 3) | (values$ws.binmean < PC.param$ws.rated)
  values <- values[OK,]
  # figure out the new values we'll add
  new.values <- data.frame(ws.binmin = seq(max(values$ws.binmin)+0.5,99.75,0.5),
                           ws.binmax = seq(max(values$ws.binmax)+0.5,100.25,0.5),                           
                           ws.binmean = seq(max(values$ws.binmin)+0.75,100,0.5))
  new.values$power.binmean = PC.param$power.rated
  new.values$ws.binnpoints = 3
  new.values$power.binsd = 0
  new.values$ti.binmean = NA
  
  return(list("values" = rbind(values,
                               new.values),
              "param" = PC.param))
}