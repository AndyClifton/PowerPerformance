#' Generate a power curve based on parameters
#' 
#' @param param is a data.frame that contains paramaeters that describe a wind turbine's performance
#' @export

PCfromParam <- function(param,
                        ws = seq(3,30,0.5)){
  # what about density?
  # assume sea-level
  rho <- 1.225
  if ("rho" %in% colnames(param)){
    # use the values we were supplied with
    rho <- param$rho
  }   
  
  # Check to see if we were given a maximum cp
  if ("cp.max" %in% colnames(param)){
    cp <- param$cp.max
  }  
  # Check to see if we were given a cp vector
  if ("cp" %in% colnames(param)){
    cp <- param$cp
  }  
  # Check to see if we were given a wind speed vector
  if ("ws" %in% colnames(param)){
    ws <- param$ws
  }  
  
  # get the power
  power.W <- 1/2 * rho * ws^3 * cp * pi * param$diameter^2 /4    
  
  # we now have our power curve; check to see if there was anything that would make us limit it
  if ("ws.cutin" %in% colnames(param)){
    # and we can only start producing at wind speeds over cutin
    power.W[ws < max(param$ws.cutin, na.rm = TRUE)] <- 0
  }  
  if (("ws.rated" %in% colnames(param))){
    # remember that we can only achieve rated power
    power.W[ws >= (max(param$ws.rated, na.rm = TRUE))] <- max(param$power.rated,na.rm = TRUE) * 1000
  }  
  if (("power.rated" %in% colnames(param))){
    # remember that we can only achieve rated power
    power.W[power.W >= (max(param$power.rated, na.rm = TRUE) * 1000)] <- max(param$power.rated,na.rm = TRUE) * 1000
  }
  if (("ws.cutout" %in% colnames(param))){
    # and we can only go up to ws.cutout
    power.W[ws > max(param$ws.cutout,na.rm = TRUE)] <- 0.0
  }  
  
  # create the output
  cbind(ws.binmean = ws,
        ws.binnpoints = rep(3.0,NROW(ws)),
        power.binmean = power.W/1000.0,
        power.binsd = rep(NA,NROW(ws)))  
}