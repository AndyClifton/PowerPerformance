#' Workhorse function to plot power curves using different types of input
#' 
#' \code{plotPC} uses \code{ggplot2} to plot power curves using either 
#' bin-averaged data or parameters that describe the power curve.
#' 
#' @param model a data.frame containing bin-averaged wind speeds and power. Must
#'   include at least the columns \code{ws.binmean} and \code{power.binmean}. 
#'   The standard deviation of power in the bin can be passed in as 
#'   \code{power.binsd}
#' @param param a set of parameters that describe the power curve
#' @return a ggplot object
#' @export
#' @family Power curve methods
#' 
plotPC <- function(model,
                   param,
                   ws = seq(3,30,0.5)){
  if(missing(model) & missing(param)){
    # then we don't have any of the data we need to plot a power curve!
  }
  if(missing(model)){
    # then assume we have the parameters that describe the power curve, and use
    # those to create the bin values differentiate between a single row of
    # parameters, and a data frame
    model = PCfromParam(param,
                        ws)
  } 
  p.PC <- ggplot(data = model,
                 aes_string(x = "ws.binmean",
                     y = "power.binmean")) +
    geom_line() +
    geom_point(size = 2)
  # check to see if we have the standard deviation in each bin
  if("power.binsd" %in% colnames(model)){
    p.PC <- p.PC +      
      geom_errorbar(aes_string(ymax = "power.binmean + power.binsd",
                        ymin = "power.binmean - power.binsd"))    
  }
  # now clean up the plot we generated
  p.PC <- p.PC +
    scale_x_continuous(name = "Wind Speed (m/s)") +
    scale_y_continuous(name = "Power (kW)")
  return(p.PC)
}