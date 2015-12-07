#' Model Luminescence Signals
#'
#' This function calculates TL, OSL an RF signals from quartz simulations.
#' The signal occurs by recombination of an electron to a luminescence center.
#'
#' @param RegDose \code{\link{numeric}} (\bold{required}): a vector with the dose points for the regeneration cycle
#'
#' @param TestDose\code{\link{numeric}} (\bold{required}): set testdose in [Gy]
#'
#' @param PH\code{\link{numeric}} (\bold{required}): set preheat temperature [°C]
#'
#' @param CH\code{\link{numeric}} (\bold{required}): set cutheat temperature [°C]
#'
#' @param Irr_temp\code{\link{numeric}} (wtih default): set irradiation temperature [°C]
#'
#' @param OSL_duration\code{\link{numeric}} (wtih default): set OSL measurement time [s]
#'
#' @param OSL_temp\code{\link{numeric}} (\bold{required}): set OSL reading temperture [°C]
#'
#' @param PH_duration\code{\link{numeric}} (wtih default): set preheat duration [s]
#'
#' @param DoseRate\code{\link{numeric}} (wtih default): set the doserate [Gy/s] of the laboratory irradiation unit
#'
#' @param optical_power\code{\link{numeric}} (wtih default):
#'
#' @return This function returns a list with a SAR sequence
#'
#' @note This function can do just nothing at the moment.
#'
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany),
#'
#' @references
#'
#' Murray, A.S. and Wintle, A.G., 2000. Luminescence dating of quartz using an
#' improved single-aliquot regenerative-dose protocol. Radiation Measurements
#' 32, 57-73.
#'
#' @seealso \code{\link{deSolve}}
#'
#' @examples
#'
#' #so far no example available
#'
#' @noRd
.RLumModel_DRT.sequence <- function(
  RegDose,
  TestDose,
  PH,
  CH,
  OSL_temp,
  Irr_2recover, 
  Irr_temp = 20,
  OSL_duration = 40,
  PH_duration = 10,
  DoseRate = 1,
  optical_power = 90
){
  
  temp.list <- list()
  sequence <- list(PH = c(240,40), 
                   ILL = c(125,400,100), 
                   IRR = c(Irr_temp,Irr_2recover,DoseRate))
  
  for (i in 1:length(RegDose)){
    
    if(RegDose[i] == 0){
      
      temp.list <-list(
        TL = c(20,PH,5),
        PAUSE = c(PH,PH_duration),
        OSL = c(OSL_temp,OSL_duration,optical_power), # Lx measurement
        IRR = c(Irr_temp,TestDose,DoseRate),
        TL = c(20,CH,5),
        OSL = c(OSL_temp,OSL_duration,optical_power) # Tx measurement
      )
      
    }
    else{
      
      temp.list <- list(
        IRR = c(Irr_temp,RegDose[i],DoseRate),
        TL = c(20,PH,5),
        PAUSE = c(PH,PH_duration),
        OSL = c(OSL_temp,OSL_duration,optical_power), # Lx measurement
        IRR = c(Irr_temp,TestDose,DoseRate),
        TL = c(20,CH,5),
        OSL = c(OSL_temp,OSL_duration,optical_power) #Tx measurement
      )
    }
    
    sequence <- c(sequence,temp.list)
    
  }
  
  return(sequence)
  
}
