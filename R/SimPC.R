#' Simulate the power produced in a bin
#' 
#' \code{SimPC} is a utility function required to calculate the zero-turbulence
#' power curve.
#' @param PC.values a data.frame containing binned wind speed and power data
#' @param PC.param an optional data.frame containing $power.rated, which is the rated power
#' @param ws wind speed
#' @param Ti turbulence intensity
#' @param rho air density (required to estimate Cp)
#' @param rel.tol relative tolerance of the integrand, expressed as ((computed - exact) / exact)
#' @return sim a data.frame containing simulated wind speed and power for this
#'   turbine
#' @export
#' 
#' @family zero-turbulence power curve methods

SimPC <- function(PC.param,
                  ws,
                  Ti,
                  rho = 1.225,
                  rel.tol = 0.001,
                  method = "simpletrap"){  
  # create an array for the results
  sim.power = rep(NA,NROW(ws))
  
  if (method == "simpletrap"){
    # do some simple numerical integration
    uintegration = seq(0,100,0.1)
    pintegration = PCfromParam(param = PC.param, 
                               ws = uintegration)[,"power.binmean"]    
  }
  
  # get the power for each value of ws
  for (i in 1:NROW(ws)){
    if (is.na(Ti[i]) == TRUE){
      sim.power[i] = PCfromParam(param = PC.param,
                                 ws = ws[i])[,"power.binmean"]
    } else {
      if ((Ti[i] == 0)){
        sim.power[i] = PCfromParam(param = PC.param,
                                   ws = ws[i])[,"power.binmean"]
      } else {
        if (method == "nintegrate"){
          sim.power[i] = integrate(integrand,
                                   lower = 0.0,
                                   upper = 100.0,        
                                   PC.param = PC.param,
                                   ws.mean = ws[i],
                                   Ti = Ti[i],
                                   subdivisions = 1000,
                                   abs.tol = rel.tol,
                                   rel.tol = rel.tol)$value
        }
        if (method == "simpletrap"){
          # apply simple trapezoidal integration, which is about 4x faster
          freq = dnorm(x = uintegration,
                       mean = ws[i],
                       sd = (Ti[i]/100.0)*ws[i])
          power = PCfromParam(param = PC.param, 
                              ws = uintegration)[,"power.binmean"]
          # keep things clear
          x = uintegration
          f = power * freq          
          # integrate using the trapezoidal rule
          n = length(x)
          sim.power[i] = 0.5*sum((x[2:n] - x[1:(n-1)]) * (f[2:n] + f[1:(n-1)]))
          weighting = 0.5*sum((x[2:n] - x[1:(n-1)]) * (freq[2:n] + freq[1:(n-1)]))
          
        }
      }
    }
  }  
  return(sim.power)
}


# create the thing we will integrate
integrand <- function(x, 
                      PC.param,
                      ws.mean,
                      Ti) {
  freq = dnorm(x = x,
               mean = ws.mean,
               sd = (Ti/100.0)*ws.mean)
  freq[is.na(freq)] = 0.0 
  freq[ws.mean <0] = 0.0
  # create the return values
  Pint = PCfromParam(param = PC.param, 
                     ws = x)[,"power.binmean"] * freq
  return(Pint)
}
