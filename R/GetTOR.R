#' Get the operating region of a wind turbine
#' 
#' \code{GetTOR} calculates the operating range of a wind
#' turbine based on the wind speed and the turbine cutin, rated and cutout wind
#' speeds.
#' 
#' @param ws a wind speed
#' @param ws.cutin the speed above which the turbine is operating
#' @param ws.rated the speed above which the turbine runs at rated power
#' @param ws.cutout the speed above which the turbine is shut down for safety
#'   (or other) reasons
#' @return TOR the turbine operating region (factor = c("I", "II", or "III"))
#'   
#' @export

GetTOR <- function(ws,
                   ws.cutin = 0.0,
                   ws.rated,
                   ws.cutout = Inf){
  TOR = rep("I",NROW(ws))
  TOR[ws < ws.cutin] = "I"
  TOR[(ws >= ws.cutin) & (ws <= ws.rated)] = "II"
  TOR[(ws > ws.rated) & (ws <= ws.cutout)] = "III"
  return(factor(TOR,
                levels = c("I","II","III"),
                ordered = TRUE))
}