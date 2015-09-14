#' Trapezoidal integration
#' 
#' \code{TrpIntegrt} is a utility function to perform trapezoidal integration.
#' @param x a vector of x data
#' @param y a vector of y data
#' @return the integral under the curve described by x and y.
#' @export

TrpIntegrt <- function(x,
                       y){
  ok <- (is.finite(x) & is.finite(y))
  x <- x[ok]
  y <- y[ok]
  
  return(sum(diff(x)*(y[-1]+y[-length(y)]))/2)
}
