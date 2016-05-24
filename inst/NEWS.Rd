\name{NEWS}
\title{NEWS for the R Package Luminescence}

\section{Changes in version 0.2.0 (, 2016)}{
\itemize{

 \subsection{New functions:}{
    \itemize{ 
      \item extract_pars2FME(): Helper function to generate the right data format for the FME-package.
      \item fit_RLumModel2data(): Fitting function for quartz luminescence model parameters to experimental data.}
}

\subsection{Bugfixes and changes:}{
\itemize{
 \item{calc_concentrations():}
    \itemize{ 
      \item Add maximum change of concentration in specific level to slot 'info' in every RLum.Data.Curve.}
      
  \item{calc_signal():}
    \itemize{ 
      \item As a result of the changes in 'set_pars' the index of the luminescence center changed. This was adjusted in 'calc_signal'.}
      
      
  \item{model_LuminescenceSignals():}

	   \itemize{
	   \item Add new parameter 'parms': Necessary for using the package FME.
	   \item Add new parameter 'own_parameters': Parameter to submit an own parameter set for simulations. For further details see
	   vignette 'RLumModel - Using own parameter sets'.
	   \item Add new parameter 'own_state_parameters': Parameter to submit own state parameters together with an own parameter set.
	   \item Add new parameter 'own_start_temperature': Parameter to submit own start temperature together with an own parameter set.}
      
    \item{set_ODE():}

	  \itemize{
	    \item Remove 'b', 'temp', 'R' and 'P' from unpacking, because these parameters are already stored in parameters.step.
	    \item Remove distinction of cases 'model = Pagonis' or 'model = Bailey' due to a misstake in Bailey 2001, 2002 and 2004. The rate equation for the valence band is wrong. Thanks to an unknown reviewer of Friedrich et al. 2016.}

  \item{set_ODE_LM_OSL():}

	   \itemize{
	   \item Remove 'a', 'b', 'temp', 'R' and 'P' from unpacking, because these parameters are already stored in parameters.step.}
	   
	 \item{set_pars():}

	   \itemize{
	   \item Changed order in the vectors N, E, s, A, B: The luminescence center is now always the last entry. 
	   This was necessary to use OTOR models with own parameters much easier.
	   \item Output is now a list and not a RLum.Results object. This is recommended for using package FME.
	   \item Add new case 'customized' for own parameter sets.}

	\item{simulate_CW_OSL():}

	  \itemize{
	  \item  Remove 'RLumModel_ID' from slot info to slot .pid. This RLumModel_ID describes the order of the command in the sequence.}
	  
	\item{simulate_irradiation():}

	  \itemize{
	  \item New distinction of cases adding the possibility to use own pair-production-rates.}

	\item{simulate_LM_OSL():}

	  \itemize{
	  \item Remove 'RLumModel_ID' from slot info to slot .pid. This RLumModel_ID describes the order of the command in the sequence.}

	\item{simulate_RF():}

	  \itemize{
	  \item Remove 'RLumModel_ID' from slot 'info' to slot .pid. This RLumModel_ID describes the order of the command in the sequence.
	  \item New distinction of cases adding the possibility to use own pair-production-rates.}

	\item{simulate_TL():}

	  \itemize{
	  \item Remove 'RLumModel_ID' from slot info to slot .pid. This RLumModel_ID describes the order of the command in the sequence.}

  \item{tests:}

	  \itemize{
	  \item Expanded tests.
	}


  \item{translate_sequence():}

	  \itemize{
	  \item Add 'sequence' to slot 'info' in returning RLum.Analysis object.
	  \item Add 'originators' to slot 'info' in returning RLum.Analysis object.
	}

}}
}
}

\section{Changes in version 0.1.0 (3rd February, 2016)}{

 \itemize{
  \item Initial version.
 }

}


