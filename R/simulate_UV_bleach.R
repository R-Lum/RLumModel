#' sequence step CW-OSL-simulation
#'
#' This function simulates the CW-OSL measurement of quartz in the energy-band-model.
#'
#' @param temp \code{\link{numeric}} (\bold{required}): temperature [deg. C] of the CW-OSL measurement
#'
#' @param duration \code{\link{numeric}} (\bold{required}): heatingrate in [deg. C/s] or [K/s]
#'
#' @param P_UV \code{\link{numeric}} (\bold{required}): optical power of UV bleach.
#'
#' @param RLumModel_ID \code{\link{numeric}} (optional): A ID-number for the CW-OSL-step. This ID
#' is pass down to \link{calc_concentrations} so all concentrations had the same ID as the
#' sequence step they were calculated from. This ID is identic to the sequence step in "sequence".
#'
#' @param n \code{\link{numeric}} or \code{\linkS4class{RLum.Results}} (\bold{required}):
#' concentration of electron-/holetraps, valence- and conduction band
#' from step before. This is necessary to get the boundary condition for the ODEs.
#'
#' @param parms \code{\linkS4class{RLum.Results}} (\bold{required}): The specific model parameters are used to simulate
#' numerical quartz luminescence results.
#'
#' @return This function returns an RLum.Results object from the CW-OSL simulation.
#'
#' @section Function version: 0.1.1
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
#' @seealso \code{\link{simualte_illumination}}
#'
#' @examples
#'
#' #so far no example available
#'
#' @noRd
.simulate_UV_bleach <- function(
  temp,
  duration,
  P_UV,
  RLumModel_ID = NULL,
  n,
  parms
){
  
  # check input arguments ---------------------------------------------------
  
  ##check if duration is a positive number
  if(duration < 0) {
    stop("\n [.simulate_UV_bleach()] Argument 'duration' has to be a positive number!")
  }
  
  ##check if n is a RLum object
  if(class(n) != "RLum.Results") {
    n <- n
  } else {
    n <- n$n
  }
  
  # Set parameters for ODE ---------------------------------------------------
  
  ##============================================================================##
  # SETTING PARAMETERS FOR ILLUMINATION
  #
  # R: electron-hole-production-rate (in Bailey 2004: 2.5e10, else: 5e7) = 0
  # P: Photonflux (in Bailey 2004: wavelength [nm]) = 1
  # b: heating rate [deg. C/s] = 0
  ##============================================================================##
  
  P <- 0
  P_UV <- P_UV
  R <- 0;
  b <- 0;
  
  temp <- temp
  
  ##============================================================================##
  # SETTING PARAMETERS FOR ODE
  ##============================================================================##
  
  times <- seq(0, duration, by = 0.1)
  parameters.step <- .extract_pars(parameters.step = list(
    R = R,
    P = P,
    P_UV = P_UV,
    temp = temp,
    b = b,
    times = times,
    parms = parms))
  
  ##============================================================================##
  # SOLVING ODE (deSolve requiered)
  ##============================================================================##
  out <- deSolve::ode(y = n, times = times, parms = parameters.step, func = .set_ODE_Rcpp, rtol=1e-3, atol=1e-3, maxsteps=1e5, method = "bdf")
  ##============================================================================##
  
  ##============================================================================##
  # CALCULATING RESULTS FROM ODE SOLVING
  ##============================================================================##
  
  signal <- .calc_signal(object = out, parameters = parameters.step)
  
  ##============================================================================##
  # CALCULATING CONCENTRATIONS FROM ODE SOLVING
  ##============================================================================##
  
  name <- c("UV_bleach")
  concentrations <- .calc_concentrations(
    data = out,
    times = times,
    name = name,
    RLumModel_ID = RLumModel_ID)
  
  ##============================================================================##
  # TAKING THE LAST LINE OF "OUT" TO COMMIT IT TO THE NEXT STEP
  ##============================================================================##
  
  return(Luminescence::set_RLum(class = "RLum.Results",
                                data = list(
                                  n = out[length(times),-1] ,
                                  UV_bleach.data = Luminescence::set_RLum(
                                    class = "RLum.Data.Curve",
                                    data = matrix(data = c(times, signal),ncol = 2),
                                    recordType = "UV_bleach",
                                    curveType = "simulated",
                                    info = list(
                                      curveDescripter = NA_character_),
                                    .pid = as.character(RLumModel_ID)
                                  ),
                                  temp = temp,
                                  concentrations = concentrations)
  )
  )
  
}#end function
