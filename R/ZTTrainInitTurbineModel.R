#' Initialize the zero-turbulence power curve
#' 
#' \code{ZTTrainInitTurbineModel} calculates the initial zero-turbulence power curve using 
#' the method set out in the draft version of IEC 61400-12-1 (2013). This should
#' be the first stage in calculating a site-specific power curve using the 
#' zero-turbulence power curve methdology. The result of this function should be
#' used as the input to \code{\link{ZTTrainTheoTurbineModel}}.
#' 
#' @param PC.values a power curve data.frame, created using 
#'   \code{\link{PCTrainTurbineModel}}
#' @param rho (optional) air density
#' @param diameter the turbine diameter
#' @return parameters describing the initial 
#'   zero-turbulence power curve
#' @export
#'   
#' @family zero-turbulence power curve methods

ZTTrainInitTurbineModel <- function(PC.values,
                                    rho = NA*NROW(PC.values),
                                    diameter = NA){
  # generate initial zero-turbulence power curve according to IEC 61400:12-1
  
  ## CUT IN ----
  # As a first approach, the cut-in wind speed is set to the average wind speed 
  # of the wind speed bin, where the measured power output reaches
  # at least 0.1 % of the rated power.  
  PC.values$above.cutin <- PC.values$power.binmean >= ((0.1/100)* max(PC.values$power.binmean,
                                                                      na.rm = TRUE))
  ws.cutin <- min(PC.values$ws.binmean[PC.values$above.cutin],
                  na.rm = TRUE)
  
  ## Cp,max ----
  # Assumption of constant power coefficient cP equal to the maximum power 
  # coefficient cP,max between cut-in wind speed and rated wind speed.
  # ...
  # As a first approach, the maximum power coefficient cP,max shall be set to 
  # the maximum power coefficient of the measured bin-averaged power curve.
  PC.values$cp.binmean = PC.values$power.binmean*1000 / 
    ((1/2)*rho*pi*(diameter^2/4) * PC.values$ws.binmean^3)
  cp.max <- max(PC.values$cp.binmean[PC.values$above.cutin],
                na.rm = TRUE)  
  
  ## RATED SPEED ----
  # Calculation of rated wind speed vrated from rated power Prated, rotor swept area A, maximum
  # power coefficient cP,max and reference air density rho, by:
  # v_rated = ((2*P_rated)/(rho*C_p,max*A))^(1/3). 
  # As a first approach, the rated power is set to the highest bin-averaged 
  # power output of all wind speed bins.
  
  # note that we have the rated power..
  ws.rated <- ((2*max(PC.values$power.binmean,
                      na.rm = TRUE)*1000)/(rho*cp.max*pi*diameter^2/4))^(1/3) 
  
  ## ZERO TURBULENCE POWER CURVE ----
  # make an initial zero turbulence power curve
  PC.values.zeroturb <- data.frame("ws" = seq(0,100,0.5),
                                   "cp" = NA,
                                   "power" = NA)
  # either operate at maximum Cp or maximum power
  PC.values.zeroturb$power <- 1/2 * rho * cp.max * pi*diameter^2/4 * 
    PC.values.zeroturb$ws^3/1000
  PC.values.zeroturb$power[PC.values.zeroturb$ws < ws.cutin] <- 0
  PC.values.zeroturb$power[PC.values.zeroturb$ws >= ws.rated] <- max(PC.values$power.binmean,
                                                                     na.rm = TRUE)
  # get the Cp above rated
  PC.values.zeroturb$cp <- PC.values.zeroturb$power*1000 / 
    ((1/2)*rho*pi*(diameter^2/4) * PC.values.zeroturb$ws^3)   
  
  # get all the data in one place so that it can be returned
  return(list("values" = data.frame(ws.binmean = PC.values.zeroturb$ws,
                                    ws.binnpoints = 1,
                                    power.binmean = PC.values.zeroturb$power,
                                    power.binsd = 0),
              "param" = data.frame('iteration' = 0,
                                   'power.rated' = max(PC.values$power.binmean,
                                                       na.rm = TRUE),
                                   'ws.cutin' = ws.cutin,
                                   'cp.max' = cp.max,
                                   'ws.rated' = ws.rated,
                                   'diameter' = diameter)))
}