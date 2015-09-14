#'Get an estimate of turbine power production from a wind speed-power curve
#'
#'\code{PCQueryTurbineModel} queries the power curve to find the power expected at a
#'given wind speed, if the wind speed is input. If the power is input, the wind
#'speed will be estimated from the power. \code{PCQueryTurbineModel} uses
#'\code{approx} to perform a linear interpolation between the input power curve
#'data points to the point of interest. Wind speeds that exceed the range of
#'data in the input power curve are assumed to have 0 mean power.
#'@param power.curve a power curve object containing information about wind
#'  speeds and power
#'@param ws the wind speed (can be a vector)
#'@param power the power (can be a vector)
#'@note either ws or power should be given as an input.
#'@return if ws is input, a vector of power at wind speed ws
#'@return if power is input, a vector of the lowest wind speed to achieve that
#'  power
#'@export
#'@family Power curve methods

PCQueryTurbineModel <- function(power.curve,
                                ws = NULL,
                                power = NULL){
  # remove NA from the power curve
  power.curve <- power.curve[!is.na(power.curve$power.binmean),]
  
  if (missing(power)){
      # interpolate between data points to get power
      power <- approx(x = power.curve$ws.binmean,
                      y = power.curve$power.binmean,
                      xout = ws,
                      yleft = NA, # return NA for x less than min(x)
                      yright = NA,# return NA for x greater than max(x)
                      ties = mean)$y    
      return(power)
  }
  if (missing(ws)){
    # apply a linear interpolation to estimate the wind speed required for this power
    ws <- approx(x = power.curve$power.binmean,
                 y = power.curve$ws.binmean,                 
                 xout = power,
                 yleft = NA, # return 0 for x less than min(x)
                 yright = NA,# return 0 for x greater than max(x)
                 ties = min)$y    
    return(ws)
  }
}
