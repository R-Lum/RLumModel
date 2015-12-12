#' sequence step illumination
#'
#' This function simulates illumination step in the energy-band-model of quartz.
#'
#' @param temp \code{\link{numeric}} (\bold{required}): set temperature [°C] of the illumination simulation
#'
#' @param duration \code{\link{numeric}} (\bold{required}): duration of the illumination simulation
#'
#' @param n \code{\link{numeric}} (\bold{required}): concentration of electron-/holetraps, valence- and conductionband
#' from step before
#'
#' @param parms \code{\link{Rlum.Results object}} (\bold{required}):
#'
#' @param \dots further arguments and graphical parameters passed to
#' \code{\link{plot.default}}. See details for further information
#'
#' @return This function returns an Rlum.Results object from the illumination simulation.
#'
#' @note This function can do just nothing at the moment.
#'
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany),
#'
#' @references
#'
#' @seealso \code{\link{plot}}
#'
#' @examples
#'
#' #so far no example available
#'
#' @noRd
.simulate_illumination <- function(
  temp,
  duration,
  optical_power = 100,
  n,
  parms,
  ...
){

  ##check if object is of class RLum.Data.Curve
  if(class(n) != "RLum.Results"){
    n <- n
  }
  else{
    n <- n$n
  }

  ##2. check if duration is a positive number
  if(duration < 0){
    stop("\n Duration has to be an positive number!")
  }

  ##============================================================================##
  # SETTING PARAMETERS FOR ILLUMINATION
  #
  # R: electron-hole-production-rate (in Bailey 2004: 2.5e10, else: 5e7) = 0
  # P: Photonflux (in Bailey 2002/2004: wavelength [nm]) = 1
  # b: heating rate [°C/s] = 0
  ##============================================================================##

  if(parms$model == "Bailey2004" || parms$model == "Bailey2002"){
    P <- 0.02/(1.6*10^(-19)*(1240/470))*(optical_power/100)
  }
  else{
    P <- 2*(optical_power/100)
  }

  R <- 0
  b <- 0

  ##============================================================================##
  # SETTING PARAMETERS FOR ODE
  ##============================================================================##

  times <- seq(0, duration, by = duration/100)
  parameters.step  <- list(R = R, P = P, temp = temp, b = b, times = times, parms = parms)

  ##============================================================================##
  # SOLVING ODE (deSolve requiered)
  ##============================================================================##
  out <- deSolve::lsoda(y = n, times = times, parms = parameters.step, func = .set_ODE, rtol=1e-3, atol=1e-3, maxsteps=1e5);
  ##============================================================================##

  ##============================================================================##
  # TAKING THE LAST LINE OF "OUT" TO COMMIT IT TO THE NEXT STEP
  ##============================================================================##

  return(set_RLum(class = "RLum.Results",
                  data = list(
                    n = out[length(times),-1],
                    temp = temp
                  )))

}#end function
