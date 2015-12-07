### ===============================================================================================
### R package RLumModel BUILDSCRIPTS
### BibTeX
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2015-12-04
### ===============================================================================================

library(tools)
library(RLumModel)

##get version number
temp <- readLines("DESCRIPTION")
temp <- temp[grep("Version", temp)]
temp.version <- sub(" ","",unlist(strsplit(temp,":"))[2])

package.citation <- toBibtex(citation("RLumModel"))
write(package.citation, file=paste0("RLumModel.BuildResults/RLumModel_", temp.version,"-bibliography.bib"))
