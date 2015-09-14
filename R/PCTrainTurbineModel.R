#' Create an IEC-conformal power curve
#' 
#' \code{PCTrainTurbineModel} creates a wind turbine power curve using the method
#' described in IEC 61400-12-1 (2005).
#' 
#' The input windspeed is assumed to have been corrected for density.
#' 
#' @param ws wind speed observations
#' @param power power observations
#' @param ti (optional) the observed turbulence intensity
#' @param power.rated (optional) the rated power of the turbine generator
#' @param ws.delta (optional, default = 0.5 m/s) the width of the bins
#' @param ws.cutin (optional, default = 3 m/s)
#' @param ws.min the minimum wind speed, below which power = 0
#' @param ws.max the maximum wind speed to bin data to
#' @return a data.frame containing \describe{\item{ws.binmin}{The minimum wind
#'   speed in that bin} \item{ws.binmax}{the maximum wind speed in that bin}
#'   \item{ws.binnpoints}{The number of observations (points) in that bin}
#'   \item{ws.binmean}{The mean wind speed in that bin} \item{ti.binmean}{The
#'   mean turbulence intensity in that bin (NA if no Ti given)}
#'   \item{power.binmean}{The mean power in that bin}
#'   \item{power.binsd}{The standard deviation of the power in that bin}}
#' @export
#' 
#' @seealso \code{\link{GetErrorMetrics}, \link{RFTrainTurbineModel}}
#' @family Power curve methods

PCTrainTurbineModel <- function(ws,
                          power,
                          ti = rep(NA,length(ws)),
                          power.rated = NA,
                          ws.delta = 0.5,
                          ws.cutin = 3,                       
                          ws.min = 0.25,
                          ws.max = (round(max(ws)/ws.delta) * ws.delta) - ws.delta){
  # generate an IEC-standard power curve using the data provided according to IEC 61400-12-1 (2005).
  
  # Figure out the range ---- 
  # According to IEC 61400-12-1,
  # "The selected data sets
  # shall at least cover a wind speed range extending from 1 m/s below cut-in to
  # 1,5 times the wind speed at 85 % of the rated power of the wind turbine" (7.6)
  # and..
  # "The databaase shall be considered complete when it has met the following criteria:
  # - each bin includes a minimum of 30 min of sampled data;
  # - the database includes a minimum of 180 h of sampled data;
  # Should a single incomplete bin be preventing completion of the test, then 
  # that bin value can be estimated by linear interpolation from the two adjacent 
  # complete bins. 
  # In order to complete the power curve at high wind speeds the following procedure can be used:
  #  - for wind speeds above 1,6 times the wind speed at 85 % of rated power 
  #         the measurement sector can be opened.
  
  # now work through the bins (note going to do this old school rather than R-style)
  ws.binmin = ws.min
  ws.binmax = ws.binmin + ws.delta
  ws.bini = 1
  while(ws.binmax < ws.max){
    # get the observations that fall in this bin
    in.bin = ((ws >= ws.binmin) & (ws < ws.binmax))
    # get the mean wind speed
    if (sum(as.numeric(in.bin))>=3){
      # based on observations
      ws.binmean = mean(ws[in.bin], na.rm = TRUE)
      ti.binmean = mean(ti[in.bin], na.rm = TRUE)
      # get the mean power  
      power.binmean = mean(power[in.bin], na.rm = TRUE)    
      # get the standard deviation of the power
      power.binsd = sd(power[in.bin], na.rm = TRUE)    
    } else {
      # not enough data
      ws.binmean = mean(c(ws.binmin,ws.binmax))
      ti.binmean = NA
      power.binmean = NA
      power.binsd = NA
    }
    if (!is.na(power.rated)){
      if (!is.na(power.binmean) & ((power.binmean > (0.85 * power.rated)) == TRUE)){
        ws.max = min(ws.max, 1.5 * ws.binmax )  
      }
    }
    # store the data
    new.data <- data.frame("ws.binmin" = ws.binmin,
                           "ws.binmax" = ws.binmax,
                           "ws.binnpoints" = sum(in.bin),
                           "ws.binmean" = ws.binmean,
                           "ti.binmean" = ti.binmean,
                           "power.binmean" = power.binmean,
                           "power.binsd" = power.binsd)
    if (ws.bini == 1){
      power.curve <- new.data
    } else {
      power.curve <- rbind(power.curve,
                           new.data)
    }
    # look at the next bin
    ws.bini = ws.bini + 1
    ws.binmin = ws.binmin + ws.delta
    ws.binmax = ws.binmax + ws.delta
  }
  return(power.curve)
}

