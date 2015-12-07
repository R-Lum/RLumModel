#' sequence step RF/RL simulation
#'
#' This function simulates the radioluminescence of quartz in the energy-band-model.
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
#' @return This function returns an Rlum.Results object of the RF/RL simulation.
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
#' Bailey, R.M., 2002. Simulations of variability in the luminescence characteristics of natural 
#' quartz and its implications for estimates of absorbed dose. 
#' Radiation Protection Dosimetry 100, 33-38.
#'
#' Bailey, R.M., 2004. Paper I-simulation of dose absorption in quartz over geological timescales
#' and it simplications for the precision and accuracy of optical dating.
#' Radiation Measurements 38, 299-310.
#'
#' Pagonis, V., Chen, R., Wintle, A.G., 2007: Modelling thermal transfer in optically
#' stimulated luminescence of quartz. Journal of Physics D: Applied Physics 40, 998-1006.
#' 
#' Pagonis, V., Wintle, A.G., Chen, R., Wang, X.L., 2008. A theoretical model for a new dating protocol
#' for quartz based on thermally transferred OSL (TT-OSL).
#' Radiation Measurements 43, 704-708.
#'
#' @seealso \code{\link{plot}}
#'
#' @examples
#'
#' #so far no example available
#'
#' @noRd
.RLumModel_RL <- function(
  temp,
  dose,
  doseRate,
  n,
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
  # CALCULATING RESULTS FROM ODE SOLVING
  ##============================================================================##
  
  signal <- .RLumModel_calcSignal(out = out, parameters = parameters.step)
  
  ##============================================================================##
  # IF ARGUMENT PLOT == TRUE
  ##============================================================================##
#   if(plot==TRUE){
#     plot(times,signal,type = "l", main = "RF",...)
#   }  #end if
  
  ##============================================================================##
  # TAKING THE LAST LINE OF "OUT" TO COMMIT IT TO THE NEXT STEP
  ##============================================================================##
  
  return(set_RLum(class = "RLum.Results",
                  data = list(
                    n = out[length(times), -1],
                    RF.data = set_RLum(
                      class = "RLum.Data.Curve",
                      data = matrix(data = c(times, signal), ncol = 2),
                      recordType = "RF",
                      curveType = "simulated"
                    ) ,
                    temp = temp
                  )))
  
}

