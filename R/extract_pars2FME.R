#' Prepare parameters for use with R package FME
#' 
#' @param model Name of the quartz luminescence model. Possible choices are: 
#' "Bailey2001", "Bailey2002, "Bailey2004", "Pagonis2007" and "Pagonis2008".
#' 
#' @return This function returns a named \code{\link{vector}} for use with R package FME 
#' 
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany),
#' 
#' @seealso \code{\link{model_LuminescenceSignals}}, \code{\link[FME]{sensFun}}, \code{\link[FME]{sensRange}}
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
#' Soetaert, K., Cash, J., Mazzia, F., 2012. Solving differential equations in R.
#' Springer Science & Business Media.
#' 
#' @examples
#' 
#' pars2FME <- extract_pars2FME(model = "Bailey2001")
#' 
#' @export
extract_pars2FME <- function(
  model
){
  
  ##load model parameters
  temp_pars <- .set_pars(model)
  
  ##prepare parms for use with FME, remove RLum.Results objects and character strings,
  ##as well as natural constants
  temp_pars$n <- NULL
  temp_pars$model <- NULL
  temp_pars$k_B <- NULL
  temp_pars$W <- NULL
  temp_pars$K <- NULL
  
  ##unlist parms for direct call, e.g. parms["N1"]
  return(unlist(temp_pars))
}