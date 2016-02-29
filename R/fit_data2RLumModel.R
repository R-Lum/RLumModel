#' Fit experimental data to model parameters
#' 
#' @param sequence \code{\link{list}} (\bold{required}): set sequence to model as \code{\link{list}} or as *.seq file from the
#' Riso sequence editor. To simulate SAR measurements there is an extra option to set the sequence list (cf. details).
#' See \code{\link{model_LuminescenceSignals}} for a detailed description.
#
#' @param model \code{\link{character}} (\bold{required}): set model to be used. Available models are:
#' "Bailey2001", "Bailey2002", "Bailey2004", "Pagonis2007", "Pagonis2008"
#'
#' @param seq.step2fit \code{\link{numeric}}: Choose the sequence step number to fit.
#' \bold{Note: Only sequence steps with an signal output are allowed! This sequence steps are:
#' RF, TL, OSL, LM-OSL}
#' 
#' @param lab.dose_rate \code{\link{numeric}} (with default): laboratory dose rate in XXX
#' Gy/s for calculating seconds into Gray in the *.seq file.
#'
#' @param simulate_sample_history \code{\link{logical}} (with default): FALSE (with default): simulation begins at laboratory conditions, TRUE: simulations begins at crystallization (all levels 0)
#' process
#' 
#' @param plot \code{\link{logical}} (with default): Enables or disables plot output. 
#' Recommended: No plot output, because fitting function will run a lot of times.
#'
#' @param verbose \code{\link{logical}} (with default): Verbose mode on/off.
#' 
#' @param \dots further arguments and graphical parameters passed to
#' \code{\link{plot.default}}. See details for further information.
#' 
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany)
#' 
#' @return This function returns a function, which is necessary for further calculations
#' with the package "FME".
#' 
#' @seealso \code{\link[FME]{sensFun}}, \code{\link[FME]{sensRange}}, 
#' \code{\link{model_LuminescenceSignals}}
#'
#' @export 
fit_data2RLumModel <- function(
  sequence,
  model,
  seq.step2fit,
  lab.dose_rate = 1,
  simulate_sample_history = FALSE,
  verbose = FALSE,
  plot = FALSE,
  ...
){
  
  # Integrity tests and conversion are done by model_LuminescenceSignals()

  
  # Load parameters --------------------------------------------------------------
  
  parms <- extract_pars2FME(model = model)
  
  .fit_RLumModel2FME <- function(parms){
    temp_out <- model_LuminescenceSignals(
      model = model,
      sequence = sequence,
      plot = plot,
      verbose = verbose, 
      parms = parms,
      ...) 

    record.id <- which(structure_RLum(temp_out)[".pid"] == as.character(seq.step2fit))
    
    data_model <- data.frame(time = temp_out@records[[record.id]]@data[,1], signal = temp_out@records[[record.id]]@data[,2]/max(temp_out@records[[record.id]]@data[,2]))
    return(data_model)
  }

}