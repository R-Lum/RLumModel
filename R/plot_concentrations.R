#' Plot electron/hole concentrations of a specific record.id
#'
#' The functions provides a plot of all changes in time of the electron respectively hole concentration
#' in electron traps, hole centres, in the condunction and valence band.
#'
#' The function produces a multiple plot output and uses in main parts the Luminescence function
#' \code{\link[Luminescence]{plot_RLum.Analysis}}.
#' A file output is recommended (e.g., \code{\link{pdf}}).
#'
#'
#' @param object \code{\linkS4class{RLum.Analysis}} (\bold{required}):  S4
#' object of class \code{RLum.Analysis}, e.g. the values of \code{\link{model_LuminescenceSignals}}.
#'
#' @param record.id \code{\link{numeric}} (\bold{required}): id of the simulated record, which
#' is to plot. To see all record.ids use \code{\link[Luminescence]{structure_RLum}}, see examples.
#'
#' @param \dots further arguments and graphical parameters passed to
#' \code{\link{plot.default}} and \code{\link{plot_RLum.Analysis}}.
#'
#' @return Returns multiple plots.
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
#' @seealso \code{\link{plot}}, \code{\link[Luminescence]{plot_RLum.Analysis}},
#' \code{\link{model_LuminescenceSignals}}
#'
#' @examples
#'
#' ##load data
#' data(ExampleData.ModelOutput, envir = environment())
#'
#' ##show structure
#' Luminescence::structure_RLum(model.output)
#'
#' ##plot all concentrations
#' plot_concentrations(object = model.output,
#'                     record.id = 1)
#'
#' ##plot only specific energy-band-level (e.g. 110 degree celsius trap, "concentration level 1")
#' plot_concentrations(object = model.output,
#'                     record.id = 1,
#'                     subset = list(recordType = "concentration level 1"))
#'
#' ##plot every level on a single plot
#' plot_concentrations(object = model.output,
#'                     record.id = 1,
#'                     plot.single = TRUE)
#'
#' @export
plot_concentrations <- function(
  object,
  record.id,
  ...
  ){

# check input arguments ---------------------------------------------------

  ##check if RLum.Analysis object
  if(class(object) != "RLum.Analysis"){
    stop("[plot_concentrations()] Input object is not of type 'RLum.Analysis'")
  }

  ##check if record.id is numeric
  if(class(record.id) != "numeric"){
    stop("[plot_concentrations()] Argument \"record.id\" is not of type 'numeric'")
  }

  ##check if record.id > 0 and > lengt(object)
  if(record.id <= 0 | record.id > length(object)){
    stop(paste("[plot_concentrations()] Argument \"record.id\" has to be non-negative and maximum",length(object)))
  }


# get concentrations ------------------------------------------------------

  temp_id <- Luminescence::get_RLum(object, record.id = record.id)
  level_concentrations <- temp_id@info$concentrations

  temp_parameters <- .set_pars(object@protocol)
  indicator <- temp_parameters$B



# main --------------------------------------------------------------

  main <- lapply(1:(length(indicator)), function(x){
    if(indicator[x] == 0){

      return(paste("Electron \n",level_concentrations@records[[x]]@recordType, sep = ""))
    }

  if(indicator[x] != 0){

      return(paste("Hole \n",level_concentrations@records[[x]]@recordType, sep = ""))
    }

  })

  ##set valence and conduction band names
  main[[length(indicator)+1]] <- "Electron concentration \n conduction band"
  main[[length(indicator)+2]] <- "Hole concentration \n valence band"

# xlab  -------------------------------------------------------------------

  if(names(object)[record.id] == "TL") {##check if "TL" is part of the record-id to plot, add xlab, ylab

    xlab <- "Temperature [\u00B0C]"

  }

  if(names(object)[record.id] == "OSL" || names(object)[record.id] == "LM-OSL" ) {##check if "TL" is part of the record-id to plot, add xlab, ylab

    xlab <- "Illumination time [s]"

  }

  if(names(object)[record.id] == "RL" || names(object)[record.id] == "RF"){

    xlab <- "Irradiation time [s]"

  }

# ylab --------------------------------------------------------------

      ylab <- expression(paste("Concentration [",1/cm^{3},"]"))

# Plot --------------------------------------------------------------------

  ##handle extra args
  extraArgs <- list(...)

  ##check if "xlab" in ...
  if("xlab" %in% names(extraArgs))
  {

    xlab <- extraArgs$xlab

  } else {

    xlab = xlab

  }

  ##check if "ylab" in ...
  if("ylab" %in% names(extraArgs))
  {

    ylab <- extraArgs$ylab

  } else {

    ylab <- ylab
    par(mar = c(5,4.5,4,2) + 0.1) #see superscript completely

  }

  ##check if "main" in ...
  if("main" %in% names(extraArgs))
  {

    main <- extraArgs$main

  } else {

    main <- main
  }


  ##check if "mtext" in ...
  if("mtext" %in% names(extraArgs))
  {

    mtext <- extraArgs$mtext

  } else {

    mtext <- ""

  }


  ##plot
  Luminescence::plot_RLum.Analysis(level_concentrations,
            xlab = xlab,
            ylab = ylab,
            main = main,
            mtext = mtext,
            ...)

}#end function
