#' Plot concentrations of electrones respectively holes of all levels from a
#' energy-band-model against time.
#'
#' The functions provides a plot of all changes in time of the electron respectively hole concentration
#' in electron traps, hole centres, in the condunction and valence band.
#'
#' The function produces a multiple plot output and uses in main parts the Luminescence function
#' "Luminescence::plot_RLum". A file output is recommended (e.g., \code{\link{pdf}}).
#'
#' @param object \code{\linkS4class{RLum.Analysis}} (\bold{required}):  S4
#' object of class \code{RLum.Analysis}, e.g. the values of \code{\link{model_LuminescenceSignals}}.
#'
#' @param record.step \code{\link{numeric}} (\bold{required}): step of the simulated record which
#' is to plot.
#'
#' @param \dots further arguments and graphical parameters passed to
#'
#' \code{\link{plot.default}}. See details for further information
#'
#' @return Returns multiple plots for the concentrations of electrones respectively holes.
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
#' @seealso \code{\link{plot}}, \code{\link{plot_RLum.Analysis}}
#'
#' @examples
#'
#' ##load data
#' data(ExampleData.ModelOutput, envir = environment())
#'
#' ##plot all concentrations
#' plot_concentrations(object = model.output, record.step = 1)
#'
#' ##plot only specific energy-band-level (e.g. 110 degree celsius trap, "concentration level 1")#'
#' plot_concentrations(object = model.output,
#'                     record.step = 1,
#'                     subset = list(recordType = "concentration level 1"))
#'
#' ##plot every level on a single plot
#' plot_concentrations(object = model.output, record.step = 1, plot.single = TRUE)
#'
#' @export
plot_concentrations <- function(object, record.step, ...){



# check input arguments ---------------------------------------------------

  ##check if RLum.Analysis object
  if(class(object) != "RLum.Analysis"){
    stop("[plot_concentrations()] Input object is not of type 'RLum.Analysis'")
  }

  ##check if record.step is numeric
  if(class(record.step) != "numeric"){
    stop("[plot_concentrations()] Argument \"record.step\" is not of type 'numeric'")
  }

  if(record.step <= 0 | record.step > length(object)){
    stop(paste("[plot_concentrations()] Argument \"record.step\" has to be non-negative and maximum",length(object)))
  }


# get concentrations ------------------------------------------------------

  level_concentrations <- object@records[[record.step]]@info$concentrations

  temp_parameters <- .set_Pars(object@protocol)
  indicator <- temp_parameters$B

# label main --------------------------------------------------------------

  main <- lapply(1:(length(indicator)), function(x){
    if(indicator[x] == 0){

      return(paste("Electron ",level_concentrations@records[[x]]@recordType, sep = ""))
    }

  if(indicator[x] != 0){

      return(paste("Hole ",level_concentrations@records[[x]]@recordType, sep = ""))
    }

  })

  ##set valence and conduction band names
  main[[length(indicator)+1]] <- "Electron concentration conduction band"
  main[[length(indicator)+2]] <- "Hole concentration valence band"



# label ylab --------------------------------------------------------------

  ylab <- lapply(1:(length(indicator)), function(x){
    if(indicator[x] == 0){

      return("Electron concentration [1/cm^3]")
    }

    if(indicator[x] != 0){

      return("Hole concentration [1/cm^3]")
    }

  })

  ##set valence and conduction band names
  ylab[[length(indicator)+1]] <- "Electron concentration [1/cm^3]"
  ylab[[length(indicator)+2]] <- "Hole concentration [1/cm^3]"


# Plot --------------------------------------------------------------------

  ##handle extra args
  extraArgs <- list(...)

  ##check if "xlab" in ...
  if("xlab" %in% names(extraArgs))
  {

    xlab <- extraArgs$xlab

  } else {

    if(names(object)[record.step] == "TL") {##check if "TL" is part of the record-step to plot, add xlab, ylab

      xlab <- "Temperature [\u00B0C]"

    } else {

      xlab <- "Stimulation time [s]"

    }
  }

  ##check if "ylab" in ...
  if("ylab" %in% names(extraArgs))
  {

    ylab <- extraArgs$ylab

  } else {

    ylab <- ylab
  }

  ##check if "main" in ...
  if("main" %in% names(extraArgs))
  {

    main <- extraArgs$main

  } else {

    main <- main
  }

  ##plot
  Luminescence::plot_RLum.Analysis(level_concentrations,
            xlab = xlab,
            ylab = ylab,
            main = main,
            ...)

}#end function
