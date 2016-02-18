\name{NEWS}
\title{NEWS for the R Package Luminescence}

\section{Changes in version 0.1.1 (, 2016)}{
\itemize{
 \item{calc_concentrations():}
    \itemize{ \item add maximum change of concentration in specific level to slot 'info'
   }
    \item{set_ODE():}

	  \itemize{
	    \item remove 'b', 'temp', 'R' and 'P' from unpacking, because these parameters are already stored in parameters.step.}

  \item{set_ODE_LM_OSL():}

	   \itemize{
	   \item remove 'a', 'b', 'temp', 'R' and 'P' from unpacking, because these parameters are already stored in parameters.step.}

	\item{simulate_CW_OSL():}

	  \itemize{
	  \item  remove 'RLumModel_ID' from slot info to slot .pid. This RLumModel_ID describes the order of the command in the sequence.}

	\item{simulate_LM_OSL():}

	  \itemize{
	  \item remove 'RLumModel_ID' from slot info to slot .pid. This RLumModel_ID describes the order of the command in the sequence.}

	\item{simulate_RF():}

	  \itemize{
	  \item remove 'RLumModel_ID' from slot info to slot .pid. This RLumModel_ID describes the order of the command in the sequence.}

	\item{simulate_TL():}

	  \itemize{
	  \item remove 'RLumModel_ID' from slot info to slot .pid. This RLumModel_ID describes the order of the command in the sequence.}


  \item{translate_sequence():}

	  \itemize{
	  \item add sequence to slot 'info' in returning RLum.Analysis object.
	}


 
}}





\section{Changes in version 0.1.0 (3rd February, 2016)}{

 \itemize{
  \item Initial version.


 }

}


