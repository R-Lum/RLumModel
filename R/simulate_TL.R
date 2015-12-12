#' sequence step TL-simulation
#'
#' This function simulates the TL measurement of quartz in the energy-band-model.
#'
#' @param temp_begin \code{\link{numeric}} (\bold{required}): initial temperature [°C] of the TL-simulation
#'
#' @param temp_begin \code{\link{numeric}} (\bold{required}): endtemperature [°C] of the TL-simulation
#'
#' @param b \code{\link{numeric}} (\bold{required}): heatingrate in [°C/s] or [K/s]
#'
#' @param n \code{\link{numeric}} (\bold{required}): concentration of electron-/holetraps, valence- and conductionband
#' from step before
#'
#' @param parms \code{\link{Rlum.Results object}} (\bold{required}):
#'
#' @param \dots further arguments and graphical parameters passed to
#' \code{\link{plot.default}}. See details for further information
#'
#' @return This function returns an Rlum.Results object from the TL simulation.
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
.simulate_TL <- function(
  temp_begin,
  temp_end,
  b,
  n ,
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

  ##============================================================================##
  # SETTING PARAMETERS FOR HEATING
  #
  # R: electron-hole-production-rate (in Bailey 2004: 2.5e10, else: 5e7) = 0
  # P: Photonflux (in Bailey 2004: wavelength [nm]) = 0
  # b: heating rate [°C/s]
  ##============================================================================##

  R <- 0
  P <- 0

  ##============================================================================##
  # SETTING PARAMETERS FOR ODE
  ##============================================================================##

  times <- seq(0, (temp_end-temp_begin)/b, by = 0.1)
  parameters.step  <- list(R = R, P = P, temp = temp_begin, b = b, times = times, parms = parms)

  ##============================================================================##
  # SOLVING ODE (deSolve requiered)
  ##============================================================================##
  out <- deSolve::lsoda(y = n, times = times, parms = parameters.step, func = .set_ODE, rtol=1e-3, atol=1e-3, maxsteps=1e5)
  ##============================================================================##

  ##============================================================================##
  # CALCULATING RESULTS FROM ODE SOLVING
  ##============================================================================##

  signal <- .calc_Signal(out = out, parameters = parameters.step)
  TSkala <- times*b+temp_begin

  ##============================================================================##
  # TAKING THE LAST LINE OF "OUT" TO COMMIT IT TO THE NEXT STEP
  ##============================================================================##

  return(set_RLum(class = "RLum.Results",
                  data = list(
                    n = out[length(times),-1],
                    TL.data = set_RLum(
                      class = "RLum.Data.Curve",
                      data = matrix(data = c(TSkala, signal),ncol = 2),
                      recordType = "TL",
                      curveType = "simulated"
                      ),
                    temp = temp_end
                  )))
}
