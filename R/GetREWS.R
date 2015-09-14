#' Calculate the area below a certain level in a rotor disk
#' 
#' \code{AreaBelow} is a utility function to calculate the  the area below a
#' certain height in the rotor disk. The height is given with respect to the
#' bottom of the rotor disk.
#' @param h the height with respect to the bottom of the disk
#' @param R the disk radius
#' @return a single value of the area in the circle, below height h.
#' @export
#' 
#' @seealso \code{\link{SliceArea}}
AreaBelow <- function(h,R){
  A = R^2*acos((R-h)/R)-(R-h)*(2*R*h-h^2)^(1/2)
  return(A)
}

#' Calculate the area of a slice through a rotor disk
#' 
#' \code{SliceArea} is a utility function to calculate the area of different slices through a turbine rotor disk
#' @param z the height above ground
#' @param zhub the height of the turbine hub
#' @param dia the turbine diameter
#' @return a data frame containing
#' \describe{\item{z}{the height of the anemometer}
#' \item{slice.lower}{the lower extent of the slice}
#' \item{slice.upper}{the upper extent of the slice}
#' \item{Ai}{the area of this slice}
#' \item{Atotal}{the total area of the rotor disk, calculated from the sum of the slice areas}
#' \item{Ageom}{the area of the rotor disk, calculated using simple geometry}
#' \item{Apercent}{the area of this slice, relative to the area of the rotor disk}
#' }
#' @export
#' 
#' @seealso \code{\link{SliceArea}}
SliceArea <- function(z,zhub,dia){  
  # clean up the inputs
  zsort = sort(z, decreasing = FALSE, index.return=TRUE)
  z <- z[zsort$ix]  
  # declare empty variables
  Ai <- c()
  zRange <- c()
  if (length(z)==1){
    if ((z > (zhub-dia/2)) & (z < (zhub + dia/2))){
      # store this
      zRange = c(zhub-dia/2,zhub+dia/2)
    }else{
      # store this
      zRange = c(NA,NA)
    }
  }else{
    for (zi in 1:(length(z))){   
      # first guess
      zlower <- NA
      zupper <- NA
      # now get it right
      if (((z[zi] > (zhub-dia/2)) & (z[zi] < (zhub + dia/2)))==TRUE){
        # within the rotor        
        if (zi == 1){
          zlower <- zhub - dia /2
          zupper <- (z[zi]+z[zi+1])/2
        } else {          
          if (zi == length(z)){
            zlower <- (z[zi-1] + z[zi])/2
            zupper <- zhub + dia /2
          } else {
            if (z[zi-1]<zhub - dia/2){
              zlower <- zhub - dia/2
            } else {
              zlower <- (z[zi-1] + z[zi])/2
            }
            if (z[zi+1]>zhub + dia/2){
              zupper <- zhub + dia/2
            } else {
              zupper <- (z[zi] + z[zi+1])/2
            }
          }
        }
        # sanity check
        zlower <- max((zhub - dia /2), zlower)
        zupper <- min((zhub + dia/2), zupper)
      } else {
        # outside the rotor plane        
      }
      # store this
      zRange = rbind(zRange,c(zlower, zupper))
    }
  }
  # get the area
  for (zi in 1:(NROW(zRange))){
    Ai[zi] = AreaBelow(zRange[zi,2]-(zhub-dia/2), dia/2) - 
      AreaBelow(zRange[zi,1]-(zhub-dia/2), dia/2)
  }
  # create outputs, noting that we sorted them at the start
  result <- data.frame(z = z,
                       slice.lower = zRange[,1],
                       slice.upper = zRange[,2],
                       Ai = Ai,                 
                       Atotal=sum(Ai,na.rm=TRUE),
                       Ageom = pi*dia^2/4,
                       Apercent = Ai / (pi*dia^2/4))
  return(result)
}

#' Calculate rotor-equivalent wind speed from a wind speed profile
#' 
#' \code{GetREWS} calculates the rotor-equivalent wind speed using the method set out in "Accounting for the speed shear in wind turbine power performance measurement" (Wind Energy, 2011). 
#' @param u a vector of wind speeds
#' @param z a vector of heights
#' @param zhub the turbine hub height
#' @param dia the turbine rotor diameter
#' @return a single value of the rotor equvalent wind speed
#' @export
#' 
#' @seealso \code{\link{GetErrorMetrics}}
GetREWS <- function(u,z,zhub,dia){
  # clean up the inputs
  # remove NA
  z <- z[!(is.na(u))]
  u <- u[!(is.na(u))]
  # sort the data
  zsort = sort(z, decreasing = FALSE, index.return=TRUE)
  data <- data.frame("z" = z[zsort$ix],
  "u" = u[zsort$ix])
  # check to see if we have all of the data or not
  if (length(data$z) == length(z)){
    # get the area of each slice    
    Areas <- SliceArea(data$z,zhub,dia)
    # cat(sprintf("Slices: %f\n",Areas$slices))
    # get the rotor-equivalent wind speed
    REWS <- sum((data$u^3 * Areas$Apercent),na.rm=TRUE)^(1/3)
  } else {
    REWS <- NA  
  }
  return(REWS)
}