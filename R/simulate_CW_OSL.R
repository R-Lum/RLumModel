#' sequence step CW-OSL-simulation
#'
#' This function simulates the CW-OSL measurement of quartz in the energy-band-model.
#'
#' @param temp \code{\link{numeric}} (\bold{required}): temperature [°C] of the CW-OSL measurement
#'
#' @param duration \code{\link{numeric}} (\bold{required}): heatingrate in [°C/s] or [K/s]
#'
#' @param optical_power \code{\link{numeric}} (\bold{with default}): optical power in % of full power of the LED
#'
#' @param n \code{\link{numeric}} (\bold{required}): concentration of electron-/holetraps, valence- and conductionband
#' from step before
#'
#' @param parms \code{\link{Rlum.Results object}} (\bold{required}):
#'
#' @param \dots further arguments and graphical parameters passed to
#' \code{\link{plot.default}}. See details for further information
#'
#' @return This function returns an Rlum.Results object from the CW-OSL simulation.
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
#' @seealso \code{\link{plot}}
#'
#' @examples
#'
#' #so far no example available
#'
#' @noRd
.simulate_CW_OSL <- function(
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

  ##1. check if duration is a positive number
  if(duration < 0){
    stop("\n Duration has to be an positive number!")
  }

  ##============================================================================##
  # SETTING PARAMETERS FOR ILLUMINATION
  #
  # R: electron-hole-production-rate (in Bailey 2004: 2.5e10, else: 5e7) = 0
  # P: Photonflux (in Bailey 2004: wavelength [nm]) = 1
  # b: heating rate [°C/s] = 0
  ##============================================================================##

  if(parms$model == "Bailey2004" || parms$model == "Bailey2002"){
    P <- 0.02/(1.6*10^(-19)*(1240/470))*(optical_power/100)
  }
  else{
    P <- 2*(optical_power/100)
  }

  R <- 0;
  b <- 0;

  ##============================================================================##
  # SETTING PARAMETERS FOR ODE
  ##============================================================================##

  times <- seq(0, duration, by = 0.1)
  parameters.step  <- list(R = R, P = P, temp = temp, b = b, times = times, parms = parms)

  ##============================================================================##
  # SOLVING ODE (deSolve requiered)
  ##============================================================================##
  out <- deSolve::lsoda(y = n, times = times, parms = parameters.step, func = .set_ODE, rtol=1e-3, atol=1e-3, maxsteps=1e5);
  ##============================================================================##

  ##============================================================================##
  # CALCULATING RESULTS FROM ODE SOLVING
  ##============================================================================##

  signal <- .calc_Signal(out = out, parameters = parameters.step)

  ##============================================================================##
  # TAKING THE LAST LINE OF "OUT" TO COMMIT IT TO THE NEXT STEP
  ##============================================================================##

  return(set_RLum(class = "RLum.Results",
                  data = list(
                    n = out[length(times),-1] ,
                    CW_OSL.data = set_RLum(
                      class = "RLum.Data.Curve",
                      data = matrix(data = c(times, signal),ncol = 2),
                      recordType = "OSL",
                      curveType = "simulated"
                    ),
                  temp = temp
                )))

}#end function
