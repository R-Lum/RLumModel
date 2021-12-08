#'@title Trace parameter state evolution
#'
#'@description Traces the evolution of the concentrations in the different
#'levels over different simulation steps. For instance, a sequence consisting
#'of one TL and one OSL step has two iterations. For each step the end concentration
#'is extracted. This way, the evolution of the system can be traced throughout
#'a sequence.
#'
#'@param object [Luminescence::RLum.Analysis-class] (**required**): input object
#'created by the function [model_LuminescenceSignals]. The input can be a list of such
#'objects
#'
#'@param step [character] (*optional*): filter the input object to pick particular
#'steps, the input is passed to [Luminescence::get_RLum]
#'
#'@param plot [logical] (*with default*): enables/disables plot output
#'
#'@param ... optional arguments to be passed to control the plot output. Supported
#'are `xlab`, `ylab`, `col`, `type`, `bg`, `main`. Where meaningful, parameters can be provided
#'as vectors. Vectors short than the number of plots are recycled.
#'
#'@return Returns a plot and [list] with [matrix] objects of the parameter evolution. If
#'`object` is a [list] the output is a nested [list]
#'
#'@section Function version: 0.1.0
#'
#'@author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#'@examples
#'
#' sequence <-
#' list(
#' IRR = c(20, 10, 1),
#' TL = c(20, 400, 5),
#' IRR = c(20, 10, 1),
#' TL = c(20, 400, 5))
#'
#'##model sequence
#'model.output <- model_LuminescenceSignals(
#'  sequence = sequence,
#'  verbose = FALSE,
#'  plot = FALSE,
#'  model = "Bailey2001")
#'
#'## trace
#'trace_ParameterStateEvolution(model.output)
#'
#'@md
#'@export
trace_ParameterStateEvolution <- function(
  object,
  step = NULL,
  plot = TRUE,
  ...
){

# Self call ---------------------------------------------------------------
  if (is(object, "list")) {
    arguments <- list(step = step[1], plot = plot[1], list(...))

    return(lapply(object, function(x){
      do.call("trace_ParameterStateEvolution", c(list(object = x), arguments))
    }))

  }

# Check incoming ----------------------------------------------------------
  if (!is(object, "RLum.Analysis"))
    stop("[trace_ParameterStateEvolution()] object is not of class 'RLum.Analysis!",
         call. = FALSE)

  if (object@originator != "model_LuminescenceSignals")
    stop("[trace_ParameterStateEvolution()] object was not produced by model_LuminescencerSignals()!",
         call. = FALSE)

# Prepare data ------------------------------------------------------------
  if (!is.null(step))
    object <- get_RLum(object, recordType = step[1], drop = FALSE)

  if (length(object) == 0)
    stop("[trace_ParameterStateEvolution()] object has length zero!", call. = FALSE)

# Extract parameter evolution ---------------------------------------------
  ## get list of parameters ... but only the names of the concentration
  param_names <- names(object)
  param_names <- unique(unlist(
    regmatches(
      x = param_names,
      m = regexec("conc\\.\\s.+(?=\\s\\()", param_names, perl = TRUE))))

  ## extract start and end values of each state
  m_list <- lapply(param_names, function(p){
    vapply(get_RLum(object, recordType = p), function(x){
          x@data[c(1,nrow(x@data)),2]
        }, numeric(2))

  })

  ## assign rownames
  m_list <- lapply(m_list, function(x){
    rownames(x) <- c("start", "end")
    x
  })

  ## assigning level names
  names(m_list) <- param_names

# Plotting ----------------------------------------------------------------
  if (plot) {
    ## obtain number of plots to produce
    n <- length(m_list)

    ## plot area
    if (n > 1) {
      op <- par(
        mfrow = c(n, 1),
        mar = c(0, 5, 0, 5),
        omi = c(0.5, 0, 0.5, 0)
      )
      on.exit(par(op))
    }

    ## set plot settings
    plot_settings <- modifyList(list(
      main = "State parameter evolution (end concentrations)",
      xlab = "Evolution index",
      ylab = regmatches(param_names, regexec("[^conc\\.].+", param_names)),
      col = khroma::colour("muted")(),
      bg = TRUE,
      type = "l"

    ), list(...))

    ## expand parameters, this saves us a lot of trouble later on
    plot_settings <- lapply(plot_settings, rep, length.out = n)

    ## print single plots
    for (i in 1:n) {
      ## odd and even change of plots
      if (i %% 2 != 0) {
        plot(NA, NA,
          xaxt = "n",
          xlab = if (n > 1) "" else plot_settings$xlab[1],
          xlim = c(1,ncol(m_list[[i]])),
          ylim = range(m_list[[i]]),
          ylab = plot_settings$ylab[i],
          frame = FALSE)

        ## add background
        if (plot_settings$bg[1]) {
          rect(
            par("usr")[1],
            par("usr")[3],
            par("usr")[2],
            par("usr")[4],
            col = rgb(0, 0, 0, 0.1),
            border = FALSE
          )
        }

        ## add lines
        lines(
          x = 1:ncol(m_list[[i]]),
          y = m_list[[i]][2,],
          type = plot_settings$type[i],
          col = plot_settings$col[i])

        ## add title
        if (i == 1)
          mtext(side = 3, plot_settings$main[1], outer = TRUE, line = 1)

      } else {
        plot(
          x = 1:ncol(m_list[[i]]),
          y = m_list[[i]][2,],
          xaxt = "n",
          yaxt = "n",
          type = plot_settings$type[i],
          frame = FALSE,
          ylab = "",
          col = plot_settings$col[i])
        axis(side = 4)
        mtext(side = 4, text = plot_settings$ylab[i], line = 3, cex = 0.7)
      }

    }

    ## add x-axis
    axis(side = 1, at = pretty(1:ncol(m_list[[1]])), las = 1)

    if (n > 1)
      mtext(side = 1, text = plot_settings$xlab[1], cex = 0.7, line = 2)
  }

# Return ------------------------------------------------------------------
 return(m_list)

}
