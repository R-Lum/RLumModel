#' Model Luminescence Signals
#'
#' This function calculates TL, OSL an RF signals from quartz simulations.
#' The signal occurs by recombination of an electron to a luminescence center.
#'
#' @param out \code{\link{matrix of class deSolve}} (\bold{required}): set sequence to model as character vector ord as *.seq file from the
#' Riso sequence editor
#'
#' @param parameters\code{\link{list}} (\bold{required}): set parameters to calculate the signal
#'
#' @return This function returns a vector with OSL/TL/RF signal per time unit.
#'
#' @note This function can do just nothing at the moment.
#'
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany),
#'
#' @references
#'
#' Bailey, R.M., 2001. Towards a general kinetic model for optically and thermally stimulated
#' luminescence of quartz. Radiation Measurements 33, 17-45.
#'
#' @seealso \code{\link{deSolve}}
#'
#' @examples
#'
#' #so far no example available
#'
#' @noRd
.RLumModel_calcSignal <- function(
  out,
  parameters
  ){

  ##============================================================================##
  ## unpack parameters to be used in this function
  ##============================================================================##

  N <- parameters$parms$N
  B <- parameters$parms$B
  k_B <- parameters$parms$k_B
  W <- parameters$parms$W
  K <- parameters$parms$K

  temp <- parameters$temp
  b <- parameters$b
  times <- parameters$times
  ##============================================================================##

#delete time-row from ODE output
out <- out[,-1]

#unname luminescence center for easier use
#luminescence center is second to last in parameters
n_L <- unname(out[,length(N)-1])

#name conduction band for easier use
#luminescence center is next to max parameters (see ODE)
n_c <- unname(out[,length(N)+1])

#calculating quenching factor
nu <- 1/(1+K*exp(-W/(k_B*(273+temp+b*times))))

#calculating signal (recombination from conduction band to L-center)
signal <- n_L*n_c*B[length(N)-1]*nu

#return Signal
return(signal)

}
