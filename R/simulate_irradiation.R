#' sequence step irradiation
#'
#' This function simulates the irradiaton of quartz in the energy-band-model.
#'
#' @param temp \code{\link{cnumeric}} (\bold{required}): temperature [°C] at which the dose should be applied
#'
#' @param dose \code{\link{numeric}} (\bold{required}): dose to apply in Gray
#'
#' @param DoseRate \code{\link{numeric}} (\bold{required}): Doserate in Gy/s
#'
#' @param n \code{\link{numeric}} or \code{\linkS4class{RLum.Results}} (\bold{required}):
#' concentration of electron-/holetraps, valence- and conduction band
#' from step before. This is necessary to get the boundary condition for the ODEs.
#'
#' @param parms \code{\linkS4class{RLum.Results}} (\bold{required}): The specific model parameters are used to simulate
#' numerical quartz luminescence results.
#'
#' @return This function returns an Rlum.Results object.
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
#' @seealso \code{\link{model_LuminescenceSignals}}, \code{\link{simulate_RL}}
#'
#' @examples
#'
#' #so far no example available
#'
#' @noRd
.simulate_irradiation <- function(
  temp,
  dose,
  DoseRate,
  n,
  parms
){

# check input arguments ---------------------------------------------------

  ##check if temperature is > 0 K (-273 degree celsius)
  if(temp < -273){
    stop("\n [.simulate_irradiation()] Argument 'temp' has to be > 0 K!")
  }
  ##check if doserate is a positive number
  if(DoseRate < 0){
    stop("\n [.simulate_irradiation()] Argument 'DoseRate' has to be a positive number!")
  }

  ##check if dose is a positive number
  if(dose < 0){
    stop("\n [.simulate_irradiation()] Argument 'dose' has to be a positive number!")
  }

  ##check if n is a RLum object
  if(class(n) != "RLum.Results"){
    n <- n
  } else {
    n <- n$n
  }

# Set parameters for ODE ---------------------------------------------------

  ##============================================================================##
  # SETTING PARAMETERS FOR IRRADIATION
  #
  # R: electron-hole-production-rate (in Bailey 2004: 2.5e10, Bailey 2002: 3e10, else: 5e7)
  # P: Photonflux (in Bailey 2004: wavelength [nm])
  # b: heating rate [°C/s]
  ##============================================================================##
  if(parms$model == "Bailey2004"){
    R <- DoseRate*2.5e10
  }
  if(parms$model == "Bailey2002"){
    R <- DoseRate*3e10
  }
  else{
    R <- DoseRate*5e7  # all other simulations
  }

  P <- 0
  b <- 0

  ##============================================================================##
  # SETTING PARAMETERS FOR ODE
  ##============================================================================##

  times   <- seq(0, dose/(DoseRate), by = (dose/DoseRate)/100)
  parameters.step  <- list(R = R, P = P, temp = temp, b = b, times = times, parms = parms)

  ##============================================================================##
  # SOLVING ODE (deSolve requiered)
  ##============================================================================##
  out <- deSolve::lsoda(y = n, times = times, parms = parameters.step, func = .set_ODE ,  rtol=1e-3, atol=1e-3, maxsteps=1e5);

  ##============================================================================##
  # TAKING THE LAST LINE OF "OUT" TO COMMIT IT TO THE NEXT STEP
  ##============================================================================##
# print(out[length(times),-1])
  return(Luminescence::set_RLum(class = "RLum.Results",
                  data = list(
                    n = out[length(times),-1],
                    temp = temp
                  )))

}

