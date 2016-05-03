#' Prepare parameters for use with R package FME and function \code{\link{fit_RLumModel2data}}
#' 
#' @param model \code{\link{character}}: set model to be used. Available models are:
#' "Bailey2001", "Bailey2002", "Bailey2004", "Pagonis2007", "Pagonis2008"
#' 
#' @param parms \code{\link{list}}: If an own parameter set is used for 'inverse modelling'.
#' 
#' @return This function returns a named \code{\link{vector}} for use with R package FME 
#' 
#' @section Function version: 0.1.0 [2016-04-29]
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany),
#' 
#' @seealso \code{\link{model_LuminescenceSignals}}, \code{\link[FME]{sensFun}}, 
#' \code{\link[FME]{sensRange}}, \code{\link{fit_RLumModel2data}}
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
#' Soetaert K., Petzoldt T., 2010: Inverse Modelling, Sensitivity and Monte Carlo Analysis in
#' R Using Package FME. 
#' Journal of Statistical Software, 33, 1-28.
#'
#' Soetaert, K., Cash, J., Mazzia, F., 2012: Solving differential equations in R.
#' Springer Science & Business Media.
#' 
#' @examples
#' 
#' ## use default model
#' parms <- extract_pars2FME(model = "Bailey2001")
#' 
#' ## use own parameter set
#' 
#' own_parameters <- list(
#'  N = c(1e13,0),
#'  E = c(1.3, 0),
#'  s = c(1e12, 0),
#'  A = c(1e-8, 0),
#'  B = c(0, 1e-8),
#'  Th = c(0, 0),
#'  E_th = c(0, 0),
#'  k_B = 8.617e-5,
#'  K = 0,
#'  model = "customized"
#' )
#' 
#' parms <- extract_pars2FME(parms = own_parameters)
#' 
#' @export
extract_pars2FME <- function(
  model = NULL,
  parms = NULL
){
  
  # Integrity tests and conversion --------------------------------------------------------------
  
  if(is.null(model) & is.null(parms)){
    
    stop("[extract_pars2FME()] Either 'model' or 'parms' has to be a function argument")
  }
  
  model.allowed_keywords <- c("Bailey2001", "Bailey2004", "Pagonis2008", "Pagonis2007", "Bailey2002")
  
  if(!is.null(model)){
    if(!model%in%model.allowed_keywords){
    stop(paste0("[model_LuminescenceSignals()] Model not supported. Supported models are: ", paste(model.allowed_keywords, collapse = ", ")))
    }
  }
  
  if(!is.null(parms) & class(parms)!= "list"){
    
    stop("[extract_pars2FME()] Function argument 'parms' has to be of class list")
  }
  
  # Function ------------------------------------------------------------------------------------ 
  
  ##load model parameters
  if(is.null(parms)){
    
    temp_pars <- .set_pars(model)
  
    ##prepare parms for use with FME, remove RLum.Results objects and character strings,
    ##as well as natural constants
    
    temp_pars$model <- NULL
    temp_pars$k_B <- NULL
    temp_pars$W <- NULL
    temp_pars$K <- NULL
    temp_pars$n <- NULL
  
    ##unlist parms for direct call, e.g. parms["N1"]
    return(unlist(temp_pars))
    
  } else { ##model = NULL
    
    parms$model <- NULL
    parms$k_B <- NULL
    parms$W <- NULL
    parms$K <- NULL
    
    ##unlist parms for direct call, e.g. parms["N1"]
    return(unlist(parms))
    
  }

}