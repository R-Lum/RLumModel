#' Create a SAR sequence for 'RLumModel'
#'
#' This function creates a SAR (single-aliquot-regeneration) sequence with special keywords
#' for luminescence dating.
#'
#' @param RegDose \code{\link{numeric}} (\bold{required}): a vector with the dose points for the regeneration cycle
#'
#' @param TestDose\code{\link{numeric}} (\bold{required}): set testdose in [Gy]
#'
#' @param PH\code{\link{numeric}} (\bold{required}): set preheat temperature [°C]
#'
#' @param CH\code{\link{numeric}} (\bold{required}): set cutheat temperature [°C]
#'
#' @param OSL_temp\code{\link{numeric}} (\bold{required}): set OSL reading temperture [°C]
#'
#' @param Irr_temp\code{\link{numeric}} (with default): set irradiation temperature [°C]
#'
#' @param OSL_duration\code{\link{numeric}} (with default): set OSL measurement time [s]
#'
#' @param PH_duration\code{\link{numeric}} (with default): set preheat duration [s]
#'
#' @param DoseRate\code{\link{numeric}} (with default): set the doserate [Gy/s] of the laboratory irradiation unit
#'
#' @param optical_power\code{\link{numeric}} (with default):
#'
#' @return This function returns a \code{\link{list}} with a SAR sequence. It can be read by \code{\link{model_LuminescenceSignals}}
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
#' @seealso \code{\link{create_DRT.sequence}}, \code{\link{model_LuminescenceSignals}}
#'
#' @examples
#'
#'   sequence <- .create_SAR.sequence(
#'    RegDose = c(0,8,14,26,32,0,8),
#'    TestDose = 5,
#'    PH = 240,
#'    CH = 200,
#'    OSL_temp = 125
#'    )
#'
#' @noRd
.create_SAR.sequence <- function(
  RegDose,
  TestDose,
  PH,
  CH,
  OSL_temp,
  Irr_temp = 20,
  OSL_duration = 40,
  PH_duration = 10,
  DoseRate = 1,
  optical_power = 90
  ){

  temp.list <- list()
  sequence <- NULL
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

    } else {

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
