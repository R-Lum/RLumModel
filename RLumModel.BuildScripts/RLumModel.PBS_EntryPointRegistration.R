### ===============================================================================================
### R package RLumModel BUILDSCRIPTS
### EntryPointRegistering
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2017-04-01
### ===============================================================================================

##this script bases on
##http://stackoverflow.com/questions/42313373/r-cmd-check-note-found-no-calls-to-r-registerroutines-r-usedynamicsymbols

##run this script only if the R version is at least 3.4.0
if(as.numeric(sessionInfo()$R$major) >= 3 &&
   as.numeric(sessionInfo()$R$minor) >= 4.0) {

  ##run registration
  RLumModel_init <-
    utils::capture.output(tools::package_native_routine_registration_skeleton("."))

  ##add header text
  header <-  c(
    "/* DO NOT CHANGE MANUALLY! */",
    "/* This file was produced by the function RLumModel.BuildScripts/RLumModel.PBS_EntryPointRegistration.R */"
  )

  ##write file
  write(x = c(header, RLumModel_init), file = "src/RLumModel_init.c")

}
