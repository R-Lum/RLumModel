### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### compact vignettes
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2015-09-30
### ===============================================================================================


file.list.vig <- list.files("vignettes/",pattern = ".pdf$")

for(i in 1:length(file.list.vig)){

tools::compactPDF(paths = paste0("vignettes/", file.list.vig[i]), 
                  gs_quality = "ebook")
  
}