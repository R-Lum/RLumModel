#' Fit model parameters to experimental data
#' 
#' @param sequence \code{\link{list}} (\bold{required}): set sequence to model as \code{\link{list}} or as *.seq file from the
#' Riso sequence editor. To simulate SAR measurements there is an extra option to set the sequence list (cf. details).
#' See \code{\link{model_LuminescenceSignals}} for a detailed description.
#
#' @param model \code{\link{character}} (\bold{required}): set model to be used. Available models are:
#' "Bailey2001", "Bailey2002", "Bailey2004", "Pagonis2007", "Pagonis2008"
#'
#' @param seq.step2fit \code{\link{numeric}} (\bold{required}): Choose the sequence step number to fit.
#' \bold{Note: Only sequence steps with an signal output are allowed! This sequence steps are:
#' RF, TL, OSL, LM-OSL}
#' 
#' @param norm \code{\link{logical}} (with default): Argument to normalize the signal. 
#' This is recommended when fitting parameters to experimental results.
#' 
#' @param lab.dose_rate \code{\link{numeric}} (with default): laboratory dose rate in XXX
#' Gy/s for calculating seconds into Gray in the *.seq file.
#'
#' @param simulate_sample_history \code{\link{logical}} (with default): FALSE (with default): simulation begins at laboratory conditions, TRUE: simulations begins at crystallization (all levels 0)
#' process#' 
#'
#' @param verbose \code{\link{logical}} (with default): Verbose mode on/off.
#' 
#' @param show_structure \code{\link{logical}} (with default): Shows the structure of the result.
#' Recommended to show record.id to analyse concentrations.
#' 
#' @param own_parameters \code{\link{list}} (with default): This argument allows the user to submit own parameter sets. The \code{\link{list}}
#' has to contain the following items:
#' \itemize{
#'  \item{N: Concentration of electron- and hole traps [cm^(-3)]}
#'  \item{E: Electron/Hole trap depth [eV}
#'  \item{s: Frequency factor [s^(-1)]}
#'  \item{A: Conduction band to electron trap and valence band to hole trap transition probability [s^(-1) * cm^(3)]. 
#'  \bold{CAUTION: Not every publication uses 
#'  the same definition of parameter A and B! See vignette "RLumModel - Usage with own parameter sets" for further details}}
#'  \item{B: Conduction band to hole centre transition probability [s^(-1) * cm^(3)].}
#'  \item{Th: Photo-eviction constant or photoionisation cross section, respectively}
#'  \item{E_th: Thermal assistence energy [eV]}
#'  \item{k_B: Boltzman constant 8.617e-05 [eV/K]}
#'  \item{W: activation energy 0.64 [eV] (for UV)}
#'  \item{K: 2.8e7 (dimensionless constant)}
#'  \item{model: "customized"}
#'  \item{R (optional): Ionisation rate (pair production rate) equivalent to 1 Gy/s [s^(-1) * cm^(-3)]}
#'  }
#' 
#' For further details see Bailey 2001, Wintle 1975, vignette "RLumModel - Using own parameter sets" 
#' and example 3. 
#' 
#' @param own_state_parameters \code{\link{numeric}} (with default): Some publications (e.g. Pagonis 2009)
#' offer state parameters. With this argument the user can submit this state parameters. \bold{Note:} 
#' You have to submit the state parameters for the conduction band and the valence band, too. For further details
#' see vignette ""RLumModel - Using own parameter sets" and example 3.
#' 
#' @param own_start_temperature \code{\link{numeric}} (with default): Parameter to control the start temperature (in deg. C) of
#' a simulation. This parameter takes effect only when 'model = "customized"' is choosen. 
#' 
#' @param plot \code{\link{logical}} (with default): Enables or disables plot output. 
#' Recommended: No plot output, because fitting function will run a lot of times.
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
#' @references 
#' 
#' Bailey, R.M., 2001. Towards a general kinetic model for optically and thermally stimulated
#' luminescence of quartz. Radiation Measurements 33, 17-45.
#' 
#' Soetaert K., Petzoldt T., 2010: Inverse Modelling, Sensitivity and Monte Carlo Analysis in
#' R Using Package FME. 
#' Journal of Statistical Software, 33, 1-28.
#' 
#' Wintle, A., 1975. Thermal Quenching of Thermoluminescence in Quartz. Geophysical Journal International 41, 107-113.
#' 
#' @seealso \code{\link[FME]{sensFun}}, \code{\link[FME]{sensRange}}, 
#' \code{\link{model_LuminescenceSignals}}, \code{\link{extract_parameters2FME}}
#' 
#' @examples 
#' 
#'  sequence <- list(
#'    TL = c(20, 450, 5),
#'    IRR = c(20, 5, 0.5),
#'    TL = c(0, 450, 5))
#'
#'  model <- "Pagonis2007"
#'  
#'  parms <- extract_parameters2FME(model = model)
#'
#'  func_FME <- fit_RLumModel2data(
#'    sequence = sequence, 
#'    model = model, 
#'    seq.step2fit = 3)
#'  
#'  SensR <- FME::sensFun(
#'    func = func_FME,
#'    parms = parms,
#'    sensvar = "signal",
#'    varscale = 1,
#'    senspar = c("N1","s1","E1"))
#'    
#'  plot(
#'    SensR, 
#'    legpos = "bottomleft", 
#'    xlab = "Temperature [\u00B0C]", 
#'    main = "Local Sensitivity Analysis TL")  
#'      
#' @export
fit_RLumModel2data <- function(
  model,
  sequence,
  seq.step2fit,
  norm = TRUE,
  lab.dose_rate = 1,
  simulate_sample_history = FALSE,
  verbose = FALSE,
  show_structure = FALSE,
  own_parameters = NULL,
  own_state_parameters = NULL,
  own_start_temperature = NULL,
  plot = FALSE,
  ...){

  
# Integrity tests and conversion are done by model_LuminescenceSignals()
  
  if(!model %in% c("customized", "customised")){

    if(!is.null(own_parameters)){
      stop("[fit_RLumModel2data()] Argument 'own_parameters' set, but argument 'model' not set to 'customized'.", call. = FALSE)    }

    if(!is.null(own_state_parameters)){
      stop("[fit_RLumModel2data()] Argument 'own_state_parameters' set, but argument 'model' not set to 'customized'.", call. = FALSE)    }

    if(!is.null(own_start_temperature)){
      stop("[fit_RLumModel2data()] Argument 'own_start_temeprature' set, but argument 'model' not set to 'customized'.", call. = FALSE)    }
  }

  
# Load parameters --------------------------------------------------------------
  
  if(model == "customized" || model == "customised"){
    
    parms <- extract_parameters2FME(parms = own_parameters)
    
  } else {
    
    parms <- extract_parameters2FME(model = model)

  }
  
  extraArgs <- list(...)
  
  ## set function to use with FME
  .fit_RLumModel2FME <- function(parms){
    temp_out <- model_LuminescenceSignals(
      model = model,
      sequence = sequence,
      lab.dose_rate = lab.dose_rate,
      simulate_sample_history = simulate_sample_history,
      plot = plot,
      verbose = verbose,
      show_structure = show_structure,
      own_state_parameters = own_state_parameters,
      own_parameters = own_parameters,
      own_start_temperature = own_start_temperature,
      parms_FME = parms,
      extraArgs) 

    record.id <- which(structure_RLum(temp_out)[".pid"] == as.character(seq.step2fit))
    
    ## get data ----
    data <- get_RLum(get_RLum(temp_out, record.id = record.id))
    
    ## check if norm = TRUE ---- 
    if(norm){
      data_model <- data.frame(time = data[,1], signal = data[,2]/max(data[,2]))
    } else {
      data_model <- data.frame(time = data[,1], signal = data[,2])
    }
    
    return(data_model)
  }

}