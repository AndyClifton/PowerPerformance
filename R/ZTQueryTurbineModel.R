#' Query the zero-turbulence power curve
#' 
#' \code{ZTQueryTurbineModel} calculates power at a specific wind speed and turbilence intensity from a zero-turbulence power curve.
#'  
#' @param PC.param a data.frame of parameters describing the initial
#'   zero-turbulence power curve, created using \code{\link{ZTTrainFinalTurbineModel}}
#' @param ws wind speed
#' @param Ti turbulence intensity
#' @param power bin-averaged power
#' @param rho density
#' @param newTi the target turbulence intensity
#' @return result a data.frame containing binned wind speed and power values
#' @export
#' 
#' @family zero-turbulence power curve methods
ZTQueryTurbineModel <- function(PC.param,
                                ws,
                                Ti,
                                power,
                                rho,
                                newws,
                                newTi){
  # preallocate a results array
  result = rep(NA,NROW(newws))
  # query the turbine model at lots of points
  for (i in 1:NROW(newws)){
    result[i] = PCQueryTurbineModel(power.curve = ZTTrainTiSpecificTurbineModel(PC.param = PC.param,
                                                                                ws = ws,
                                                                                Ti = Ti,
                                                                                power = power,
                                                                                rho = 1.225,
                                                                                newTi = newTi[i]),
                                    ws = newws[i])
    cat(paste("Analyzing point", i, "of",NROW(newws),"\n"))
  }
  return(result)
}