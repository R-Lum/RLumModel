#' Set parameters for different quartz luminescence models
#'
#' @param model \code{\link{character}} (\bold{required}): set model to be used
#'
#' @return This function returns an RLum.Results object with all neccessary parameters for
#' the used model.
#'
#' @note This function can do just nothing at the moment.
#'
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany),
#'
#' @examples
#'
#' pars <- RlumModel_setPars("Bailey2001")
#'
#' @noRd
.RLumModel_setPars <- function(model){

  ##============================================================================##
  ## natural constants
  ##============================================================================##

  # Boltzmann constant [ev/K]
  k_B <- 8.617e-5

  # activiation Energy [eV]
  W <- 0.64

  # dimensionless constant (for Details see Wintle (1975))
  K <- 2.8e7

  ##============================================================================##
  ## Model parameters
  ##
  ## N: concentrations of electron/hole traps [cm^(-3)]
  ## E: depth of the electron/hole trap [eV]
  ## s: frequency factor [s^(-1)]
  ## A: conduction band to electron/hole trap transition probability [s^(-1)]
  ## B: valence band to hole trap transition probability [s^(-1)]
  ## Th: photo-ionisation cross-section [s^(-1)]
  ## E_th: `thermal assistance' energy [eV]
  ## n: concentrations of electron/hole traps after sample history [cm^(-3)]
  ##============================================================================##

  parameter.list = list(

    Bailey2001 = list(

      N = c(1.5e7, 1e7, 1e9, 2.5e8, 5e10, 3e8, 1e10, 1e11, 5e9),
      E = c(0.97, 1.55, 1.7, 1.72, 2, 1.43, 1.75, 5, 5),
      s = c(5e12, 5e14, 5e13, 5e14, 1e10, 5e13, 5e14, 1e13, 1e13),
      A = c(1e-8, 1e-8, 1e-9, 5e-10, 1e-10, 5e-7, 1e-9, 1e-9, 1e-10),
      B = c(0, 0, 0, 0, 0, 5e-9, 5e-10, 1e-10, 1e-10),
      Th = c(0.75, 0, 6, 4.5, 0),
      E_th = c(0.1, 0, 0.1, 0.13, 0),
      n =  set_RLum(class = "RLum.Results", data = list(n = c(9.170143e-03, 7.619609e+04, 1.291574e+08, 7.432312e+06, 2.690420e+10, 5.741114e+06, 6.779303e+07, 2.680821e+10 , 1.591231e+08, 1.929967e-07, 3.158202e-07), temp = 20), originator = "sample history Bailey2001")
    ),

    Bailey2002 = list(

      N = c(9e9, 1e9, 1.5e11, 5e10, 1e11, 1.5e11, 2e12, 5e12, 1.2e11, 1e12, 1e13,5e11),
      E = c(0.97, 1.55, 1.7, 1.72, 1.8, 1.65, 2.6, 2, 1.43, 1.75, 5, 5),
      s = c(5e12, 5e14, 5e12, 5e13, 5e13, 5e13, 5e13, 1e10, 5e13, 5e14, 1e13, 1e13),
      A = c(1e-8, 1e-8, 1e-9, 8e-10, 8e-10, 5e-10, 2e-10, 1e-10, 5e-8, 1e-9, 1e-9, 1e-10),
      B = c(0, 0, 0, 0, 0, 0, 0, 0, 5e-9, 5e-10, 1e-10, 1e-10),
      Th = c(1e-19, 0, 1e-16, 3e-17, 4e-18, 3e-19, 2e-21, 0),
      E_th = c(0.1, 0, 0.1, 0.13, 0.2, 0.2, 0.2,0),
      n =  set_RLum(class = "RLum.Results", data = list(n = c(2.231286e+00, 1.009119e+07, 1.705852e+10, 4.363260e+09, 9.242267e+09, 6.457175e+09, 1.989090e+12, 4.981360e+12, 1.200000e+11, 1.000000e+12, 1.000000e+13, 5.000000e+11, 1.638903e-05, 4.888349e+13), temp = 20), originator = "sample history Bailey2002")
    ),
    
    Bailey2004 = list(

      N = c(1.42e10, 1.5e9, 2.05e11, 7.04e10, 1.7e11, 2.53e11, 3.58e12, 1.28e13, 4.16e12, 4.2e11, 8.83e13, 1.15e14),
      E = c(0.97, 1.55, 1.7, 1.72, 1.8, 1.65, 2.6, 2, 1.75, 1.43, 5, 5),
      s = c(5e12, 5e14, 5e12, 5e13, 5e13, 5e13, 5e13, 1e10, 5e14,  5e13, 1e13, 1e13),
      A = c(1e-8, 1e-8, 1e-9, 8e-10, 8e-10, 5e-10, 2e-10, 1e-10, 1e-9, 5e-8, 1e-9, 1e-10),
      B = c(0, 0, 0, 0, 0, 0, 0, 0, 5e-10, 5e-9, 1e-10, 1e-10),
      Th = c(1e-19, 0, 1e-16, 3e-17, 4e-18, 3e-19, 2e-21, 0),
      E_th = c(0.1, 0, 0.1, 0.13, 0.2, 0.2, 0.2, 0),
      n =  set_RLum(class = "RLum.Results", data = list(n = c(9.941874e-01, 7.901951e+07, 1.044370e+08, 2.988593e+07, 2.531635e+09, 3.960548e+10, 1.412544e+12, 2.849834e+12, 7.322997e+10, 2.685892e+07, 3.907439e+12, 5.203548e+11, 6.867563e-07, 2.094339e-08), temp = 20), originator = "sample history Bailey2004")
    ),

    Pagonis2007 = list(

      N = c(5.1e9, 1e7, 1e11, 2.5e8, 5e10, 3e8, 1e10, 1e8, 5e9),
      E = c(0.97, 1.55, 1.73, 1.8, 2, 1.43, 1.75, 5, 5),
      s = c(5e12, 5e14, 36e13, 1.5e13, 1e10, 5e13, 5e14, 1e13, 1e13),
      A = c(1e-8, 1e-9, 0.5e-9, 5e-10, 1e-10, 5e-7, 1e-9, 1e-9, 1e-10),
      B = c(0, 0, 0, 0, 0, 5e-9, 5e-10, 1e-10, 1e-10),
      Th = c(0.75, 0, 6, 4.5, 0),
      E_th = c(0.1, 0, 0.1, 0.13, 0),
      n =  set_RLum(class = "RLum.Results", data = list(n = c(3.789080e+00, 1.931745e+03, 7.323427e+08, 3.235508e+06, 1.899088e+10, 3.000000e+08, 1.000000e+10, 1.000000e+08, 5.000000e+09, 3.553295e-06, 4.326457e+09), temp = 20), originator = "sample history Pagonis2007")
    ),

    Pagonis2008 = list(

      N = c(1.5e7, 1e7, 4e7, 2.5e8, 5e10, 5e9, 4e9, 3e8, 1e10, 3e10, 1.2e12),
      E = c(0.97, 1.55, 1.73, 1.8, 2.0, 1.65, 1.6, 1.43, 1.75, 5.0, 5.0),
      s = c(5.0e12, 5.0e14, 6.5e13, 1.5e13, 1.0e10, 6.5e13, 5.0e12, 5.0e13, 5.0e14, 1.0e13, 1.0e13),
      A = c(1e-08, 1e-08, 5e-09, 5e-10, 1e-10, 1e-11, 6e-12, 5e-07, 1e-09, 1e-10, 1e-14),
      B = c(0, 0, 0, 0, 0, 0, 0,5e-09, 5e-10, 1e-10, 3e-10),
      Th = c(0.75, 0.00, 6.00, 4.50, 0.00, 0.01, 0.00),
      E_th = c(0.10, 0.00, 0.10, 0.13, 0.00, 0.20, 0.00),
      n =  set_RLum(class = "RLum.Results", data = list(n = c(3.399692e-03, 5.716744e+04, 2.879418e+07, 1.232160e+08, 2.554548e+10, 3.879612e+06, 7.550873e+06, 1.734914e+08, 3.336225e+09, 2.207005e+10, 1.292057e+08, 6.354525e-08, 3.146862e-05), temp = 20), originator = "sample history Pagonis2007")
    )
  )


  if(model == "Bailey2001"){
    return(set_RLum(class = "RLum.Results", data = list(N = parameter.list$Bailey2001$N, E = parameter.list$Bailey2001$E, s = parameter.list$Bailey2001$s, A = parameter.list$Bailey2001$A, B = parameter.list$Bailey2001$B, Th = parameter.list$Bailey2001$Th, E_th = parameter.list$Bailey2001$E_th, n = parameter.list$Bailey2001$n, k_B = k_B, W = W, K = K, model = model)))
  }
  if(model == "Bailey2002"){
    return(set_RLum(class = "RLum.Results", data = list(N = parameter.list$Bailey2002$N, E = parameter.list$Bailey2002$E, s = parameter.list$Bailey2002$s, A = parameter.list$Bailey2002$A, B = parameter.list$Bailey2002$B, Th = parameter.list$Bailey2002$Th, E_th = parameter.list$Bailey2002$E_th, n = parameter.list$Bailey2002$n, k_B = k_B, W = W, K = K, model = model)))
  }
  if(model == "Bailey2004"){
    return(set_RLum(class = "RLum.Results", data = list(N = parameter.list$Bailey2004$N, E = parameter.list$Bailey2004$E, s = parameter.list$Bailey2004$s, A = parameter.list$Bailey2004$A, B = parameter.list$Bailey2004$B, Th = parameter.list$Bailey2004$Th, E_th = parameter.list$Bailey2004$E_th, n = parameter.list$Bailey2004$n, k_B = k_B, W = W, K = K, model = model)))
  }
  if(model == "Pagonis2007"){
    return(set_RLum(class = "RLum.Results", data = list(N = parameter.list$Pagonis2007$N, E = parameter.list$Pagonis2007$E, s = parameter.list$Pagonis2007$s, A = parameter.list$Pagonis2007$A, B = parameter.list$Pagonis2007$B, Th = parameter.list$Pagonis2007$Th, E_th = parameter.list$Pagonis2007$E_th, n = parameter.list$Pagonis2007$n, k_B = k_B, W = W, K = K, model = model)))
  }
  if(model == "Pagonis2008"){
    return(set_RLum(class = "RLum.Results", data = list(N = parameter.list$Pagonis2008$N, E = parameter.list$Pagonis2008$E, s = parameter.list$Pagonis2008$s, A = parameter.list$Pagonis2008$A, B = parameter.list$Pagonis2008$B, Th = parameter.list$Pagonis2008$Th, E_th = parameter.list$Pagonis2008$E_th, n = parameter.list$Pagonis2008$n, k_B = k_B, W = W, K = K, model = model)))
  }
}
