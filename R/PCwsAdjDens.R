#' Apply a density correction to wind speed according to IEC 61400-12-1 (2005)
#' 
#' \code{PCwsAdjDens} applies the wind speed corection described in IEC 
#' 61400-12-1 (2005) for active power controlled turbines (i.e. pitch controlled
#' turbines).
#' 
#' Density is rounded to the nearest 0.05 kg m\eqn{^{-3}}, except in the region of
#' \code{rho.ref} \eqn{\pm} 0.05 kg m\eqn{^{-3}}.
#' 
#' @param ws observed wind speed
#' @param rho observed density
#' @param rho.ref the reference density to use (defaults to 1.225 kg m\eqn{^{-3}})
#' @param rho.round the value to round to (defaults to 0.05 kg m\eqn{^{-3}})
#' @return ws.adj the adjusted wind speed
#' @export
#' @family Power curve methods

PCwsAdjDens <- function(ws,
                        rho,
                        rho.ref = 1.225,
                        rho.round = 0.05){
  # round the density to the nearest rho.round
  rho.rounded <- round(rho/rho.round) * rho.round
  # except where we are within +/- rho.round of the reference density
  no.adj = abs(rho.rounded-rho.ref) < rho.round
  rho.rounded[no.adj] = rho[no.adj]
  # apply the correction
  return(ws.adj = ws*(rho.rounded / rho.ref)^(1/3))
  
}