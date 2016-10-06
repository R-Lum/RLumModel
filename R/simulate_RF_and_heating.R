#' sequence step heating/cooling between different simulation steps
#'
#' This function simulates the heating/cooling of quartz in the energy-band-model.
#'
#' @param temp_begin \code{\link{numeric}} (\bold{required}): initial temperature [deg. C] of the TL-simulation
#'
#' @param temp_begin \code{\link{numeric}} (\bold{required}): endtemperature [deg. C] of the TL-simulation
#'
#' @param heating_rate \code{\link{numeric}} (\bold{required}): heatingrate in [deg. C/s] or [K/s]
#'
#' @param n \code{\link{numeric}} or \code{\linkS4class{RLum.Results}} (\bold{required}):
#' concentration of electron-/holetraps, valence- and conduction band
#' from step before. This is necessary to get the boundary condition for the ODEs.
#'
#' @param parms \code{\linkS4class{RLum.Results}} (\bold{required}): The specific model parameters are used to simulate
#' numerical quartz luminescence results.
#'
#' @param \dots further arguments and graphical parameters passed to
#' \code{\link{plot.default}}. See details for further information
#'
#' @return This function returns an RLum.Results object from the heating/cooling simulation.
#'
#' @section Function version: 0.1.0 [2016-10-06]
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany)
#'
#' @references
#'
#' Bailey, R.M., 2001. Towards a general kinetic model for optically and thermally stimulated
#' luminescence of quartz. Radiation Measurements 33, 17-45.
#'
#' @seealso \code{\link{simulate_TL}}, code{\link{simulate_RF}}
#'
#' @examples
#'
#' #so far no example available
#'
#' @noRd
.simulate_RF_and_heating <- function(
  temp_begin,
  temp_end,
  heating_rate,
  dose_rate,
  RLumModel_ID = NULL,
  n,
  parms
){
  
  # check input arguments ---------------------------------------------------
  
  ##check if heatingrate has the rigth algebraic sign
  if((temp_begin < temp_end && heating_rate < 0)||(temp_begin > temp_end & heating_rate > 0)){
    stop("\n [.simulate_RF_and_heating()] Heatingrate has the wrong algebraic sign!")
  }
  
  ##check if temperature is > 0 K (-273 degree celsius)
  if(temp_begin < -273 ||temp_end < -273){
    stop("\n [.simulate_RF_and_heating()] Argument 'temp' has to be > 0 K!")
  }
  
  ##check if dose_rate is a positive number
  if(dose_rate < 0){
    stop("\n [.simulate_RF_and_heating()] Argument 'dose_rate' has to be a positive number!")
  }
  
  ##check if object is of class RLum.Results
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
    # b: heating rate [deg. C/s]
    ##============================================================================##
    ## check if R is given in customized parameter sets
    if("R" %in% names(parms) && parms$R != 0){
      
      R <- dose_rate*parms$R
      
    } else {
      
      if(parms$model == "Bailey2004"){
        R <- dose_rate*2.5e10
      } else {
        
        if(parms$model == "Bailey2002"){
          R <- dose_rate*3e10
        } else {
          R <- dose_rate*5e7  # all other simulations
        }
      }
    }
    
    P <- 0
    b <- heating_rate
    
    ##============================================================================##
    # SETTING PARAMETERS FOR ODE
    ##============================================================================##
    
    times <- seq(from = 0, to = (temp_end-temp_begin)/b, by = 0.1)
    parameters.step <- .extract_pars(parameters.step = list(
      R = R,
      P = P,
      temp = temp_begin,
      b = b,
      times = times,
      parms = parms))
    
    ##============================================================================##
    # SOLVING ODE (deSolve requiered)
    ##============================================================================##
    
    out <- deSolve::lsoda(y = n, times = times, parms = parameters.step, func = .set_ODE_Rcpp, rtol = 1e-6, atol = 1e-6);
    
    ##============================================================================##
    # CALCULATING RESULTS FROM ODE SOLVING
    ##============================================================================##
    
    
    signal <- .calc_signal(object = out, parameters = parameters.step)
    TSkala <- times*b+temp_begin
    
    ##============================================================================##
    # CALCULATING CONCENTRATIONS FROM ODE SOLVING
    ##============================================================================##
    
    name <- c("RF_heating")
    concentrations <- .calc_concentrations(
      data = out,
      times = TSkala,
      name = name,
      RLumModel_ID = RLumModel_ID)
    
    ##============================================================================##
    # TAKING THE LAST LINE OF "OUT" TO COMMIT IT TO THE NEXT STEP
    ##============================================================================##
    
    return(Luminescence::set_RLum(class = "RLum.Results",
                                  data = list(
                                    n = out[length(times),-1] ,
                                    RF_heating.data = Luminescence::set_RLum(
                                      class = "RLum.Data.Curve",
                                      data = matrix(data = c(TSkala, signal),ncol = 2),
                                      recordType = "RF_heating",
                                      curveType = "simulated",
                                      info = list(
                                        curveDescripter = NA_character_),
                                      .pid = as.character(RLumModel_ID)
                                    ),
                                    temp = temp_end,
                                    concentrations = concentrations)
    )
    )
    
}#end function
