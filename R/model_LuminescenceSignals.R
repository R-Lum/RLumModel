#' Model Luminescence Signals
#'
#' This function models luminescence signals for quartz based on published physical models.
#' It is possible to simulate TL, (CW-) OSL, RF measurements in a arbitrary sequence. This
#' sequence is definded as a list of certain abrivations. Furthermore it is possible to
#' load a sequence direct from the Riso Sequence Editor.
#' The output is an RLum.Analysis object and so the plots are done by the plot_RLum.Analysis
#' function. If a SAR sequence is simulated the plot output can be disabled and SAR analyse functions
#' can be used.
#'
#'
#' Defining a \bold{sequence}\cr
#'
#' \tabular{lll}{
#' \bold{Abrivation} \tab \code{Description} \tab \code{Arguments}\cr
#' TL \tab thermally stimulated luminescence \tab 'temp begin', 'temp end', 'heating rate'\cr
#' OSL\tab optically stimulated luminescence \tab 'temp', 'duration','optical power'\cr
#' LM_OSL\tab linear modulated OSL \tab 'temp', 'duration'\cr
#' RL/RF\tab radioluminescence\tab 'temp','dose', 'doserate' \cr
#' IRR\tab irradiation \tab 'temp','dose', 'doserate' \cr
#' CH \tab cutheat \tab 'temp', 'duration' \cr
#' PH  \tab preheat \tab 'temp', 'duration' \cr
#' PAUSE \tab pause \tab 'temp', 'duration'
#' }
#'
#' @param sequence \code{\link{list}} (\bold{required}): set sequence to model as list or as *.seq file from the
#' Riso sequence editor. To simulate SAR measurements there is an extra option to set the sequence list (cf. example 3):
#' (\bold{required}): RegDose: \code{\link{numeric}}, TestDose: \code{\link{numeric}}, PH: \code{\link{numeric}},
#' CH: \code{\link{numeric}}, OSL_temp: \code{\link{numeric}}. With default are: DoseRate: \code{\link{numeric}},
#' Irr_temp: \code{\link{numeric}}, optical_power: \code{\link{numeric}}, OSL_duration: \code{\link{numeric}}, PH_duration: \code{\link{numeric}}
#'
#' @param model \code{\link{character}} (\bold{required}): set model to be used
#'
#' @param lab.doseRate \code{\link{numeric}} (with default): laboratory dose rate in XXX Gy/s for calculating seconds into Gray in the *.seq file.
#'
#' @param simulate_sample_history \code{\link{logical}} (with default): FALSE (with default): simulation begins at labour conditions, TRUE: simulations begins at crystallization (all levels 0)
#' process
#'
#' @param plot \code{\link{logical}} (with default): Enables or disables plot output
#'
#' @param verbose \code{\link{logical}} (with default): Verbose mode on/off
#'
#' @param \dots further arguments and graphical parameters passed to
#' \code{\link{plot.default}}. See details for further information
#'
#' @return This function returns an RLum.Analysis object with all TL, (LM-) OSL and RF/RL steps
#' in the sequence. Every entry is a RLum.Data.Curve obejct and can be plotted, analysed etc. with
#' further RLum functions.
#'
#' @note This function can do just nothing at the moment.
#'
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany),
#' Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
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
#' @seealso \code{\link{plot}}
#'
#' @examples
#'
#' \dontrun{
#' ##================================================================##
#' ## Example 1: Simulate sample history of Bailey2001
#' ## (cf. Bailey, 2001, Fig. 1)
#' ##===============================================================##
#'
#' ##set sequence with the following steps
#' ## (1) Irraditation at 20 deg. C with a dose of 1000 Gy and a dose rate of 1 Gy/s
#' ## (2) Preheat to 350 deg. C and hold for 10 s
#' ## (3) Illumination at 200 deg. C. for 100 s with 100 % optical power
#' ## (4) Irradiation at 220 deg. C with a dose of 20 Gy and a dose rate of 0.01 Gy/s
#' ## (5) Irradiation at 20 deg. C with a dose of 10 Gy and a dose rate of 1 Gy/s
#' ## (6) TL from 20-400 deg. C with a rate of 5 K/s
#' sequence <-
#'   list(
#'    IRR = c(20, 1000, 1),
#'     PH = c(350, 10),
#'     ILL = c(200, 100, 100),
#'     IRR = c(220, 20, 0.01),
#'     IRR = c(20, 10, 1),
#'     TL = c(20, 400, 5)
#'   )
#'
#' ##model sequence
#' model.output <- model_LuminescenceSignals(
#'   sequence = sequence,
#'   model = "Bailey2001",
#'   simulate_sample_history = TRUE
#' )
#'
#'
#' ##============================================================================##
#' ## Example 2: Simulate sequence at labour without sample history
#' ##============================================================================##
#'
#' ##set sequence with the following steps
#' ## (1) Irraditation at 30 deg. C with a dose of 100 Gy and a dose rate of 1 Gy/s
#' ## (2) Preheat to 2000 deg. C and hold for 10 s
#' ## (3) LM-OSL at 125 deg. C. for 1000 s
#' ## (4) OSL at 20 deg. C for 100 s with 90 % optical power
#' ## (5) Cutheat at 220 deg. C
#' ## (6) Irradiation at 20 deg. C with a dose of 10 Gy and a dose rate of 1 Gy/s
#' ## (7) Pause at 200 deg. C for 100 s
#' ## (8) TL from 20-400 deg. C with a heat rate of 5 K/s
#' ## (9) Radiolumiescence at 20 deg. C with a dose of 20 Gy and a dose rate of 1 Gy/s
#'
#' sequence <-
#'  list(
#'    IRR = c(20, 100, 1),
#'    PH = c(200, 10),
#'    LM_OSL = c(125, 100),
#'    CH = c(200),
#'    IRR = c(20, 10, 1),
#'    PAUSE = c(200, 100),
#'    OSL = c(125, 100, 90),
#'    PAUSE = c(200, 100),
#'    TL = c(20, 400, 5), 
#'    RF = c(20, 200, 0.01)
#' )
#'
#' # call function "model_LuminescenceSignals", set sequence = sequence, model = "Pagonis2008" (palaeodose = 200 Gy)
#' # and simulate_sample_history = FALSE (default), because the sample history is not part of the sequence
#'
#' model.output <- model_LuminescenceSignals(
#'    sequence = sequence,
#'    model = "Pagonis2008"
#'    )
#'
#'
#'
#' ##============================================================================##
#' ## Example 3: Simulate SAR sequence
#' ##============================================================================##
#'
#' ##set SAR sequence with the following steps
#' ## (1) RegDose: set regenerative dose [Gy] as vector
#' ## (2) TestDose: set test dose [Gy]
#' ## (3) PH: set preheat temperature in deg. C
#' ## (4) CH: Set cutheat temperature in deg. C
#' ## (5) OSL_temp: set OSL reading temperature in deg. C
#' ## (6) OSL_duration: set OSL reading duration in s
#'
#' sequence <- list(
#'  RegDose = c(0,10,20,50,90,0,10),
#'  TestDose = 5,
#'  PH = 240,
#'  CH = 200,
#'  OSL_temp = 125,
#'  OSL_duration = 70)
#'
#' # call function "model_LuminescenceSignals", set sequence = sequence, model = "Pagonis2007" (palaeodose = 20 Gy)
#' # and simulate_sample_history = FALSE (default), because the sample history is not part of the sequence
#'
#'  model.output <- model_LuminescenceSignals(
#'
#'  sequence = sequence,
#'  model = "Pagonis2007",
#'  plot = FALSE
#'  )
#'
#' # in environment is a new object "model.output" with the results of every step of the given sequence.
#' # Plots are done at OSL and TL steps and the growth curve
#'
#' # call "analyse_SAR.CWOSL" from RLum package
#'  results <- analyse_SAR.CWOSL(model.output,
#'                             signal.integral.min = 1,
#'                             signal.integral.max = 15,
#'                             background.integral.min = 601,
#'                             background.integral.max = 701,
#'                             fit.method = "EXP",
#'                             dose.points = c(0,10,20,50,90,0,10))
#'
#'
#' ##============================================================================##
#' ## Example 4: generate sequence from *.seq file and run SAR simulation
#' ##============================================================================##
#'
#' ## call function "model_LuminescenceSignals", load *.seq file for sequence, set model = "Bailey2002" (palaeodose = 10 Gy)
#' ## and simulate_sample_history = FALSE (default), because the sample history is not part of the sequence
#'
#' model.output <- model_LuminescenceSignals(
#'   sequence = "inst/extdata/sample_SAR_cycle.SEQ",
#'   model = "Bailey2002",
#'   plot = FALSE
#' )
#'
#'
#' ## call RLum package function "analyse_SAR.CWOSL" to analyse the simulated SAR cycle
#'
#' results <- analyse_SAR.CWOSL(model.output,
#'                              signal.integral.min = 1,
#'                              signal.integral.max = 10,
#'                              background.integral.min = 601,
#'                              background.integral.max = 701,
#'                              dose.points = c(0,5,10,20,50,5,0),
#'                              fit.method = "EXP")
#'
#' print(get_RLum(results))
#'
#'
#' ##============================================================================##
#' ## Example 5: compare different optical powers of stimulation light
#' ##============================================================================##
#'
#' ## call function "model_LuminescenceSignals", model = "Bailey2004"
#' ## and simulate_sample_history = FALSE (default), because the sample history is not part of the sequence
#' ## the optical_power of the LED is varied and then compared.
#'
#' optical_power <- seq(from = 0,to = 100,by = 20)
#'
#' model.output <- lapply(1:length(optical_power), function(x){
#'
#' sequence <- list(IRR = c(20, 50, 1),
#'                   PH = c(220, 10, 5),
#'                   OSL = c(125, 50, optical_power[x])
#'                   )
#'
#' return(model_LuminescenceSignals(
#'        sequence = sequence,
#'        model = "Bailey2004",
#'        plot = FALSE
#'        ))
#' })
#'
#' ##combine output curves
#' model.output.merged <- merge_RLum(model.output)
#'
#' ##plot
#' plot_RLum(
#'  object = model.output.merged,
#'  xlab = "Illumination time [s]",
#'  ylab = "OSL signal [a.u.]",
#'  main = "OSL signal dependency on optical power of stimulation light",
#'  legend.text = paste("Optical power density", 20*optical_power/100, "mW/cm^2"),
#'  combine = TRUE)
#'
#'}
#' @export
model_LuminescenceSignals <- function(
  model,
  sequence,
  lab.doseRate = 1,
  simulate_sample_history = FALSE,
  plot = TRUE,
  verbose = TRUE,
  ...
) {


# Integrity tests and conversion --------------------------------------------------------------

  #1 Check if model is supported
  model.allowed_keywords <- c("Bailey2001", "Bailey2004", "Pagonis2008", "Pagonis2007", "Bailey2002")

  if(!model%in%model.allowed_keywords){
    stop(paste0("[model_LuminescenceSignals()] Model not supported. Supported models are: ", paste(model.allowed_keywords, collapse = ", ")))

  }

  #2 Check sequence
  if(is(sequence,"character")){

      sequence <- .RLumModel_seq2R(file = sequence, lab.doseRate = lab.doseRate)

  }

  else if(is.list(sequence)){

    if(!is.numeric(unlist(sequence))){
      stop("[model_LuminescenceSignals()] Sequence comprises non-numeric arguments!")
    }

    if("RegDose"%in%names(sequence)){# test if .RLumModel_SAR.sequence is requiered

      RegDose = sequence$RegDose
      TestDose = sequence$TestDose
      PH = sequence$PH
      CH = sequence$CH
      OSL_temp = sequence$OSL_temp

      Irr_temp = sequence$Irr_temp
      if(is.null(Irr_temp)){

        Irr_temp <- 20
      }

      OSL_duration = sequence$OSL_duration
      if(is.null(sequence$OSL_duration)){

        OSL_duration <- 40
      }

      PH_duration = sequence$PH_duration
      if(is.null(PH_duration)){

        PH_duration <- 10
      }

      DoseRate = sequence$DoseRate
      if(is.null(DoseRate)){

        DoseRate <- 1
      }

      optical_power = sequence$optical_power
      if(is.null(optical_power)){

        optical_power <- 90
      }

      sequence <- .RLumModel_SAR.sequence(
        RegDose = sequence$RegDose,
        TestDose = sequence$TestDose,
        PH = sequence$PH,
        CH = sequence$CH,
        OSL_temp = sequence$OSL_temp,
        Irr_temp = Irr_temp,
        OSL_duration = OSL_duration,
        PH_duration = PH_duration,
        DoseRate = DoseRate,
        optical_power = optical_power
        )

    }else{

      sequence <- sequence
    }
  }

  else{

    stop("[model_LuminescenceSignals()] Sequence has to be of class list or a *.seq file")
  }


  #check for wrong elements in the sequence

    ##allowed keywords
    sequence.allowed_keywords <- c("IRR","PH", "CH", "TL", "OSL", "PAUSE", "LM_OSL", "RL", "RF", "ILL")

    ##check
    if(!all(names(sequence)%in%sequence.allowed_keywords)){
      stop(paste0("[model_LuminescenceSignals()] Unknow sequence arguments: Allowed arguments are: ", paste(sequence.allowed_keywords, collapse = ", ")))

    }
    
  #check if lab.doseRate > 0
    if(lab.doseRate <= 0){
      
      stop("[model_LuminescenceSignals()] lab.doseRate has to be a positive number! ")
      
    }

# Load model parameters ------------------------------------------------------------------------------------

    parms <- .RLumModel_setPars(model)
    if(simulate_sample_history == TRUE){
      n <- set_RLum(class = "RLum.Results", data = list(n = rep(0,length(parms$N)+2), temp = 20, model = model))
    }
    else{
      n <- parms$n
    }


# sequence ------------------------------------------------------------------------------------

  #sequence, n and parms as arguments for the SequenceTranslator, who translates the sequence to different model steps
  model.output <- .RLumModel_SequenceTranslator(sequence = sequence, n = n, parms = parms)


# Plot settings -------------------------------------------------------------------------------

  if(plot){

    plot_RLum(model.output)
  }


# return model.output -------------------------------------------------------------------------
  return(model.output)

}#end function
