## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.pos = 'H', fig.align = 'center')

## ---- echo=FALSE, message = FALSE----------------------------------------
library(RLumModel)
library(knitr)
library(kableExtra)

## ---- eval = FALSE-------------------------------------------------------
#  data("ExampleData.ModelOutput", package = "RLumModel")
#  
#  ##show class
#  class(model.output)
#  
#  ##show structure
#  Luminescence::structure_RLum(model.output)
#  
#  ##seperate TL-curve from TL-concentrations
#  TL_curve <- Luminescence::get_RLum(model.output, recordType = "TL$")
#  TL_conc <- Luminescence::get_RLum(model.output, recordType = "(TL)", drop = FALSE)
#  
#  ##also possible: TL_curve <- get_RLum(model.output, record.id = 1)
#  
#  ##plot results
#  Luminescence::plot_RLum(TL_curve)
#  Luminescence::plot_RLum(TL_conc)
#  

## ---- eval = FALSE, fig.align = "center"---------------------------------
#  ##plot every energy-level by an extra plot
#  Luminescence::plot_RLum(TL_conc, plot.single = TRUE)

## ---- eval = FALSE-------------------------------------------------------
#  ##see structure of model.output
#  Luminescence::structure_RLum(model.output)

## ------------------------------------------------------------------------
model <- "Bailey2001"

## ------------------------------------------------------------------------
sequence <- system.file(
  "extdata",
  "example_SAR_cycle.SEQ",
  package = "RLumModel")

## ------------------------------------------------------------------------
lab.dose_rate <-  0.105

## ---- echo = FALSE-------------------------------------------------------
keywords <- data.frame(
  ARGUMENTS = c("TL","OSL","ILL","LM_OSL", "RF", "RF_heating", "IRR", "CH", "PH", "PAUSE"),
DESCRIPTION = c("Thermally stimulated luminescence", "Optically stimulated luminescence", "Illumination", "Linear modulated OSL", "Radiofluorescence", "RF during heating/cooling","Irradiation", "Cutheat", "Preheat", "Pause"),
SUB_ARGUMENTS = c("’temp_begin’ [°C], ’temp_end’ [°C], ’heating_rate’ [°C/s]",
"’temp’ [°C], ’duration’ [s], ’optical_power’ [%]",
"’temp’ [°C], ’duration’ [s], ’optical_power’ [%]",
"’temp’ [°C], ’duration’ [s], optional: ’start_power’ [%], ’end_power’ [%]",
"’temp’ [°C], ’dose’ [Gy], ’dose_rate’ [Gy/s]",
"’temp_begin’ [°C], ’temp_end’ [°C], 'heating rate' [°C/s], 'dose_rate' [Gy/s]",
"’temp’ [°C], ’dose’ [Gy], ’dose_rate’ [Gy/s]",
"’temp’ [°C], optional: ’duration’ [s], ’heating_rate’ [°C/s]",
"’temp’ [°C], ’duration’ [s], optional: ’heating_rate’ [°C/s]",
"’temp’ [°C], ’duration [s]’"))

kable(keywords,
      format = "html",
      col.names = c("ARGUMENTS", "DESCRIPTION", "SUB-ARGUMENTS"),
      caption = "Keywords in RLumModel for creating sequences") %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)


## ------------------------------------------------------------------------
sequence <- list(
 IRR = c(temp = 20, dose = 10, dose_rate = 1),
 TL = c(temp_begin = 20, temp_end = 400 , heating_rate = 5))


## ------------------------------------------------------------------------
sequence <- list(
  IRR = c(20, 10, 1),
  TL = c(20, 400, 5))

## ---- echo = FALSE-------------------------------------------------------
SAR_sequence_table <- data.frame(
  ABBREVIATION = c("RegDose", "TestDose", "PH", "CH", "OSL_temp", "OSL_duration", "Irr_temp", "PH_duration", "dose_rate", "optical_power", "Irr_2recover"),
  DESCRIPTION = c("Dose points of the regenerative cycles [Gy]", 
                  "Test dose for the SAR cycles [Gy]",
                  "Temperature of the preheat [°C]",
                  "Temperature of the cutheat [°C]",
                  "Temperature of OSL read out [°C]",
                  "Duration of OSL read out [s]",
                  "Temperature of irradiation [°C]",
                  "Duration of the preheat [s]",
                  "Dose rate of the laboratory irradiation source [Gy/s]",
                  "Percentage of the full illumination power [%]",
                  "Dose to be recovered in a dose-recovery-test [Gy]"),
  EXAMPLE_ARGUMENTS = c("c(0, 80, 140, 260, 320, 0, 80)",
                        "50",
                        "240",
                        "200",
                        "125",
                        "default: 40",
                        "default: 20",
                        "default: 10",
                        "default: 1",
                        "default: 90",
                        "20"))

kable(SAR_sequence_table,
      format = "html",
      col.names = c("ABBREVIATION", "DESCRIPTION", "EXAMPLE ARGUMENTS"),
      caption = "Keywords in RLumModel for creating SAR sequences") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)

## ------------------------------------------------------------------------
sequence <- list(
  RegDose = c(0,10,20,50,90,0,10),
  TestDose = 2,
  PH = 220,
  CH = 220,
  OSL_temp = 125,
  Irr_2recover = 20)

## ------------------------------------------------------------------------
sequence <- list (
IRR = c (20 , 10 , 1) ,
TL = c (20 , 400 , 5))

