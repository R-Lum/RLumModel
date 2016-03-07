\name{NEWS}
\title{NEWS for the R Package Luminescence}

\section{Changes in version 0.1.1 (, 2016)}{
\itemize{

 \subsection{New functions:}{
    \itemize{ 
      \item extract_pars2FME: Helper function to generate the right data format for the FME-package
      \item fit_RLumModel2data: Fitting function for quartz luminescence model parameters to experimental data}
}

\subsection{Bugfixes and changes:}{
\itemize{
 \item{calc_concentrations():}
    \itemize{ 
      \item Add maximum change of concentration in specific level to slot 'info' in every RLum.Data.Curve}
      
      
  \item{model_LuminescenceSignals():}

	   \itemize{
	   \item Add new parameter "parms": Necessary for use with package FME
	   \item Recalculate parameters, if parameter "parms" is used: Necessary for use with package FME}
      
    \item{set_ODE():}

	  \itemize{
	    \item Remove 'b', 'temp', 'R' and 'P' from unpacking, because these parameters are already stored in parameters.step.}

  \item{set_ODE_LM_OSL():}

	   \itemize{
	   \item Remove 'a', 'b', 'temp', 'R' and 'P' from unpacking, because these parameters are already stored in parameters.step.}
	   
	 \item{set_pars():}

	   \itemize{
	   \item Output is now a list and not a RLum.Results object. This is better for use with package FME}

	\item{simulate_CW_OSL():}

	  \itemize{
	  \item  Remove 'RLumModel_ID' from slot info to slot .pid. This RLumModel_ID describes the order of the command in the sequence.}

	\item{simulate_LM_OSL():}

	  \itemize{
	  \item Remove 'RLumModel_ID' from slot info to slot .pid. This RLumModel_ID describes the order of the command in the sequence.}

	\item{simulate_RF():}

	  \itemize{
	  \item Remove 'RLumModel_ID' from slot info to slot .pid. This RLumModel_ID describes the order of the command in the sequence.}

	\item{simulate_TL():}

	  \itemize{
	  \item Remove 'RLumModel_ID' from slot info to slot .pid. This RLumModel_ID describes the order of the command in the sequence.}

  \item{tests:}

	  \itemize{
	  \item Expanded tests.
	}


  \item{translate_sequence():}

	  \itemize{
	  \item Add sequence to slot 'info' in returning RLum.Analysis object.
	  \item Add originators to slot 'info' in returning RLum.Analysis object.
	}

}}
}
}

\section{Changes in version 0.1.0 (3rd February, 2016)}{

 \itemize{
  \item Initial version.
 }

}


