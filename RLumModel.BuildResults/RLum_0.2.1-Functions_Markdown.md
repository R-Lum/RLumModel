

| Name                      | Title                                                                              | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                | Version   | m.Date     | m.Time   | Author                                                                                                                                     | Citation   |
|:--------------------------|:-----------------------------------------------------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:----------|:-----------|:---------|:-------------------------------------------------------------------------------------------------------------------------------------------|:-----------|
| ExampleData.ModelOutput   | Example data (TL curve) simulated with parameter set from Pagonis 2007             | Example data (TL curve) simulated with parameter set from Pagonis 2007                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | 0.1.1     | NA         | NA       | Johannes Friedrich, University of Bayreuth (Germany) -                                                                                  | NA         |
| model_LuminescenceSignals | Model Luminescence Signals                                                         | This function models luminescence signals for quartz based on published physical models. It is possible to simulate TL, (CW-) OSL, RF measurements in a arbitrary sequence. This sequence is definded as a  list  of certain abrivations. Furthermore it is possible to load a sequence direct from the Riso Sequence Editor. The output is an  RLum.Analysis object and so the plots are done by the  plot_RLum.Analysis  function. If a SAR sequence is simulated the plot output can be disabled and SAR analyse functions can be used. | 0.1.3     | 2017-04-14 | 16:57:26 | Johannes Friedrich, University of Bayreuth (Germany), -  Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France) -  | NA         |
| read_SEQ2R                | Parse a Risoe SEQ-file to a sequence neccessary for simulating quartz luminescence | A SEQ-file created by the Risoe Sequence Editor can be imported to simulate the sequence written in the sequence editor.                                                                                                                                                                                                                                                                                                                                                                                                                   | 0.1.0     | 2017-04-14 | 16:57:26 | Johannes Friedrich, University of Bayreuth (Germany), -                                                                                 | NA         |
