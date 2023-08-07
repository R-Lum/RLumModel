

| Name                          | Title                                                                            | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | Version | m.Date | m.Time | Author                                                                                                                                                    | Citation                                                                                                                                                                                                                                                                                                                                                      |
|:------------------------------|:---------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:--------|:-------|:-------|:----------------------------------------------------------------------------------------------------------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| .set_pars                     | Set parameters for Different Quartz Luminescence Models                          | This function provides all necessary model parameters to the calculation of the ODEs.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | 0.1.3   | NA     | NA     | Johannes Friedrich, University of Bayreuth (Germany), -                                                                                                | Friedrich, J., 2023. .set_pars(): Set parameters for Different Quartz Luminescence Models. Function version 0.1.3. In: Friedrich, J., Kreutzer, S., Schmidt, C., 2023. RLumModel: Solving Ordinary Differential Equations to Understand Luminescence. R package version 0.2.11.9000-5. https://CRAN.R-project.org/package=RLumModel                           |
| ExampleData.ModelOutput       | Example data (TL curve) simulated with parameter set from Pagonis 2007           | Example data (TL curve) simulated with parameter set from Pagonis 2007                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 | 0.1.1   | NA     | NA     | Johannes Friedrich, University of Bayreuth (Germany) -                                                                                                 | NA                                                                                                                                                                                                                                                                                                                                                            |
| model_LuminescenceSignals     | Model Luminescence Signals                                                       | This function models luminescence signals for quartz based on published physical models. It is possible to simulate TL, (CW-) OSL, RF measurements in a arbitrary sequence. This sequence is defined as a  list  of certain aberrations. Furthermore it is possible to load a sequence direct from the Risø Sequence Editor. The output is an  Luminescence::RLum.Analysis  object and so the plots are done by the  Luminescence::plot_RLum.Analysis  function. If a SAR sequence is simulated the plot output can be disabled and SAR analyse functions can be used. | 0.1.6   | NA     | NA     | Johannes Friedrich, University of Bayreuth (Germany), -  Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom) -  | Friedrich, J., Kreutzer, S., 2023. model_LuminescenceSignals(): Model Luminescence Signals. Function version 0.1.6. In: Friedrich, J., Kreutzer, S., Schmidt, C., 2023. RLumModel: Solving Ordinary Differential Equations to Understand Luminescence. R package version 0.2.11.9000-5. https://CRAN.R-project.org/package=RLumModel                          |
| read_SEQ2R                    | Parse a Risø SEQ-file to a sequence necessary for simulating quartz luminescence | A SEQ-file created by the Risø Sequence Editor can be imported to simulate the sequence written in the sequence editor.                                                                                                                                                                                                                                                                                                                                                                                                                                                | 0.1.1   | NA     | NA     | Johannes Friedrich, University of Bayreuth (Germany), -                                                                                                | Friedrich, J., 2023. read_SEQ2R(): Parse a Risø SEQ-file to a sequence necessary for simulating quartz luminescence. Function version 0.1.1. In: Friedrich, J., Kreutzer, S., Schmidt, C., 2023. RLumModel: Solving Ordinary Differential Equations to Understand Luminescence. R package version 0.2.11.9000-5. https://CRAN.R-project.org/package=RLumModel |
| trace_ParameterStateEvolution | Trace parameter state evolution                                                  | Traces the evolution of the concentrations in the different levels over different simulation steps. For instance, a sequence consisting of one TL and one OSL step has two iterations. For each step the end concentration is extracted. This way, the evolution of the system can be traced throughout a sequence.                                                                                                                                                                                                                                                    | 0.1.0   | NA     | NA     | Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom) -                                                              | Kreutzer, S., 2023. trace_ParameterStateEvolution(): Trace parameter state evolution. Function version 0.1.0. In: Friedrich, J., Kreutzer, S., Schmidt, C., 2023. RLumModel: Solving Ordinary Differential Equations to Understand Luminescence. R package version 0.2.11.9000-5. https://CRAN.R-project.org/package=RLumModel                                |
