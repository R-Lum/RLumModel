#' Plot electron/hole concentrations of a specific record.id
#'
#' The functions provides a plot of all changes in time of the electron respectively hole concentration
#' in electron traps, hole centres, in the condunction and valence band.
#'
#' The function produces a multiple plot output and uses in main parts the Luminescence function
#' \code{\link[Luminescence]{plot_RLum.Analysis}}.
#' A file output is recommended (e.g., \code{\link{pdf}}).
#'
#' @param object \code{\linkS4class{RLum.Analysis}} (\bold{required}):  S4
#' object of class \code{RLum.Analysis}, e.g. the values of \code{\link{model_LuminescenceSignals}}.
#'
#' @param pid \code{\link{numeric}} (\bold{required}): pid of the simulated record, which
#' is to plot. To see all pids use \code{\link[Luminescence]{structure_RLum}}, see examples.
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
#'                     pid = 2)
#'
#' ##plot only specific energy-band-level (e.g. 110 degree celsius trap, "concentration level 1")
#' plot_concentrations(object = model.output,
#'                     pid = 2,
#'                     subset = list(recordType = "conc. level 1"))
#'
#' ##plot every level on a single plot
#' plot_concentrations(object = model.output,
#'                     pid = 2,
#'                     plot.single = TRUE)
#'
#' @export
plot_concentrations <- function(
  object,
  pid,
  ...
  ){

# check input arguments ---------------------------------------------------

  ##check if object is RLum.Analysis object
  if(class(object) != "RLum.Analysis"){
    stop("[plot_concentrations()] Input object is not of type 'RLum.Analysis'")
  }

  ##check if record.id is numeric
  if(class(pid) != "numeric"){
    stop("[plot_concentrations()] Argument \"pid\" is not of type 'numeric'")
  }
  
  allowed_pid <- as.character(seq(1,length(object))) %in% structure_RLum(object)$.pid

  ##check if pid is correct
  if(!(as.character(pid) %in% structure_RLum(object)$.pid)){
    stop(paste("[plot_concentrations()] Argument \"pid\" has a wrong value. Allowed:",paste(which(allowed_pid == TRUE), collapse = ", ")))
  }


# get concentrations ------------------------------------------------------

concentrations <- lapply(1:length(object), function(x){
  
  if(grepl(paste0(as.character(pid),"_"), object@records[[x]]@.pid)){
    return(object@records[[x]])
  }
  
})

##remove NULL entries in concentrations
concentrations <- concentrations[unlist(lapply(concentrations,length)!=0)]

# get indicator 

model <- object@protocol

indicator <- .set_pars(model)$B

# main --------------------------------------------------------------

  main <- lapply(1:length(indicator), function(x){

    #subset short version of "conc." with "concentration"
    title <- sub("conc.", "concentration \n", concentrations[[x]]@recordType)

    if(indicator[x] == 0){

      return(paste("Electron ",title, sep = ""))
      }

    if(indicator[x] != 0){

      return(paste("Hole ",title, sep = ""))
      }

  })

  ##set valence and conduction band names
  main[[length(indicator)+1]] <- "Electron concentration \n conduction band"
  main[[length(indicator)+2]] <- "Hole concentration \n valence band"

# xlab  -------------------------------------------------------------------

  temp_index <- structure_RLum(object)$.pid
  
  index <- which(as.character(pid) == temp_index)
  
  ##check if "TL" is part of the record-id to plot, add xlab, ylab
  if(names(object)[index] == "TL") {

    xlab <- "Temperature [\u00B0C]"

  }
  
  ##check if "TL" is part of the record-id to plot, add xlab, ylab
  if(names(object)[index] == "OSL" || names(object)[index] == "LM-OSL" ) {

    xlab <- "Illumination time [s]"

  }

  ##check if "RL" is part of the record-id to plot, add xlab, ylab
  if(names(object)[index] == "RL" || names(object)[index] == "RF"){

    xlab <- "Irradiation time [s]"

  }

# ylab --------------------------------------------------------------

  ylab <- expression(paste("Concentration [ ",1/cm^{3},"]"))

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

  ##create RLum.Analysis object
  concentrations_RLum.Analysis <- set_RLum(class = "RLum.Analysis", records = concentrations)
  
  Luminescence::plot_RLum(concentrations_RLum.Analysis,
            xlab = xlab,
            ylab = ylab,
            main = main,
            mtext = mtext,
            ...)

}#end function
