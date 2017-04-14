#!/bin/bash
#
# =================================================================================================
# RLum.CHECK_AND_BUILD shell script
# author: RLumModel ... Sebastian Kreutzer
# date: 2017-04-01
#
# Customized R check and build routine for the R package 'RLumModel'
# =================================================================================================
#
#
# CONFIG AND DEFINITIONS
# =================================================================================================
#
export TERM=xterm
PATHPACKAGE=$(dirname $0)
#
check_status(){
  if [ $? == 0 ]; then
    echo "[OK]"
  else
    echo "[FAILED]"
  fi
}
#
#
# REMOVING UNWANTED FILES
# =================================================================================================
echo ""
echo "[PREPARE FOR PACKAGE CHECK]"
echo ""
#

  echo -ne "-> Clean RLumModel.BuildResults folder ... \t"
  find ${PATHPACKAGE}/RLumModel.BuildResults -type f -exec rm {} \;
  check_status

  echo -ne "-> Remove .DS_Store ... \t\t\t"
  find ${PATHPACKAGE} -name ".DS_Store" -depth -exec rm {} \;
  check_status

  echo -ne "-> Remove .Rhistory ... \t\t\t"
  find ${PATHPACKAGE} -name ".Rhistory" -depth -exec rm {} \;
  check_status

  echo -ne "-> Remove .RData ... \t\t\t\t"
  find ${PATHPACKAGE} -name ".RData" -depth -exec rm {} \;
  check_status

  echo -ne "-> Remove NAMESPACE ... \t\t\t"
  find ${PATHPACKAGE} -name "NAMESPACE" -depth -exec rm {} \;
  check_status



# roxygen2
# =================================================================================================
  echo -ne "-> Create empty NAMESPACE file... \t\t"
  echo '# Generated by roxygen2 (4.1.0.9001): do not edit by hand' > NAMESPACE
  check_status

  echo -ne "-> Build documentation ... \t\t\t"
  eval R CMD BATCH --no-timing ${PATHPACKAGE}/RLumModel.BuildScripts/RLumModel.PBS_roxygen2.R /dev/null
  check_status


#
# COMPILE FUNCTION PARAMTER LIST
# =================================================================================================

  echo -ne "-> Compile function argument list ...\t\t"
  eval R CMD BATCH ${PATHPACKAGE}/RLumModel.BuildScripts/RLumModel.PBS_Function_Arguments.R /dev/null
  check_status

# Set entry points
# =================================================================================================
  echo -ne "-> Set entry points ... \t\t\t"
  eval R CMD BATCH --no-timing ${PATHPACKAGE}/RLumModel.BuildScripts/RLumModel.PBS_EntryPointRegistration.R /dev/null
  check_status


#
# NEWS
# =================================================================================================

  echo -ne "-> Build ASCII NEWS ... \t\t\t"
  eval R CMD BATCH --no-timing ${PATHPACKAGE}/RLumModel.BuildScripts/RLumModel.PBS_NEWS.R /dev/null
  check_status

#
# BUILD PACKAGE
# =================================================================================================
echo ""
echo "[BUILD PACKAGE]"
echo ""

  eval R CMD BUILD ${PATHPACKAGE}

#
# CHECK PACKAGE
# =================================================================================================
echo ""
echo "[CHECK PACKAGE]"
echo ""

  eval R CMD check --timings --as-cran ${PATHPACKAGE}/RLumModel*.tar.gz

  echo -ne 'Example timing warnings...:\n\n'
  eval R CMD BATCH ${PATHPACKAGE}/RLumModel.BuildScripts/RLumModel.PBS_Timings.R /dev/null
  cat ${PATHPACKAGE}/RLumModel.BuildResults/RLumModel-Ex.timings.*.WARNING


#
# INSTALL PACKAGE
# =================================================================================================
echo ""
echo "[INSTALL PACKAGE]"
echo ""

  eval R CMD INSTALL --build ${PATHPACKAGE}/RLumModel*.tar.gz

#
# COPY FILES AND CLEANING UP
# =================================================================================================
echo ""
echo "[OUTRO]"
echo ""

  echo -ne "-> Write BibTeX ... \t\t\t\t"
  eval R CMD BATCH --no-timing ${PATHPACKAGE}/RLumModel.BuildScripts/RLumModel.PBS_BibTeX.R /dev/null
  check_status

  echo -ne "-> Build function list ... \t\t\t"
  eval R CMD BATCH --no-timing ${PATHPACKAGE}/RLumModel.BuildScripts/RLumModel.PBS_Function_List.R /dev/null
  check_status

  echo -ne "-> Moving packge source files (*.tar.gz) ... \t"
  mv RLumModel_*.tar.gz RLumModel.BuildResults/ &>/dev/null
  check_status

  echo -ne "-> Moving packge compiles package (*.tgz) ... \t"
  mv RLumModel_*.tgz RLumModel.BuildResults/ &>/dev/null
  check_status

  echo -ne "-> Copy manual ... \t\t\t\t"
  cp RLumModel.Rcheck/RLumModel-manual.pdf RLumModel.BuildResults/RLumModel-manual.pdf &>/dev/null
  check_status

  echo -ne "-> Copy check results ... \t\t\t"
  cp RLumModel.Rcheck/RLumModel-Ex.pdf RLumModel.BuildResults/RLumModel-Ex.pdf &>/dev/null
  check_status

  echo -ne "-> Remove RLumModel.Rcheck ... \t\t\t"
  rm -r ${PATHPACKAGE}/RLumModel.Rcheck &>/dev/null
  check_status

  echo ""
  echo "[FINE]"
