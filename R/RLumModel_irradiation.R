#' sequence step irradiation
#'
#' This function simulates the irradiaton of quartz in the energy-band-model.
#'
#' @param temp \code{\link{cnumeric}} (\bold{required}): temperature [°C] at which the dose should be applied
#'
#' @param dose \code{\link{numeric}} (\bold{required}): dose to apply in Gray
#'
#' @param doesRate \code{\link{numeric}} (\bold{required}): named list with model parameters. Note:
#' not every parameter apply to every model, see details for further information
#'
#' @param n \code{\link{numeric}} (\bold{required}): concentration of electron-/holetraps, valence- and conductionband
#' from step before
#'
#' @param parms \code{\link{Rlum.Results object}} (\bold{required}):
#'
#' @param \dots further arguments and graphical parameters passed to
#' \code{\link{plot.default}}. See details for further information
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
#' @seealso \code{\link{plot}}
#'
#' @examples
#'
#' #so far no example available
#'
#' @noRd
.RLumModel_irradiation <- function(
  temp,
  dose,
  doseRate,
  n ,
  parms,
  ...

){


  if(!exists("parms")){
    stop("\n No parameters had been loaded!")
  }


  ##1. check if n is a RLum object
  if(class(n) != "RLum.Results"){
    n <- n
  }
  else{
    n <- n$n
  }

  ##2. check if doserate is a positive number
  if(doseRate < 0){
    stop("\n Doserate has to be an positive number!")
  }

  ##3. check if dose is a positive number
  if(dose < 0){
    stop("\n Dose has to be an positive number!")
  }



  ##============================================================================##
  # SETTING PARAMETERS FOR IRRADIATION
  #
  # R: electron-hole-production-rate (in Bailey 2004: 2.5e10, else: 5e7)
  # P: Photonflux (in Bailey 2004: wavelength [nm])
  # b: heating rate [°C/s]
  ##============================================================================##
  if(parms$model == "Bailey2004"){
    R <- doseRate*2.5e10
  }
  if(parms$model == "Bailey2002"){
    R <- doseRate*3e10
  }
  else{
    R <- doseRate*5e7  # all other simulations
  }

  P <- 0
  b <- 0

  ##============================================================================##
  # SETTING PARAMETERS FOR ODE
  ##============================================================================##

  times   <- seq(0, dose/(doseRate), by = (dose/doseRate)/100)
  parameters.step  <- list(R = R, P = P, temp = temp, b = b, times = times, parms = parms)

  ##============================================================================##
  # SOLVING ODE (deSolve requiered)
  ##============================================================================##
  out <- deSolve::lsoda(y = n, times = times, parms = parameters.step, func = .RLumModel_ODE ,  rtol=1e-3, atol=1e-3, maxsteps=1e5);

  ##============================================================================##
  # TAKING THE LAST LINE OF "OUT" TO COMMIT IT TO THE NEXT STEP
  ##============================================================================##

  return(set_RLum(class = "RLum.Results",
                  data = list(
                    n = out[length(times),-1],
                    temp = temp
                  )))

}

