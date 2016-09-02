# RLumModel

[![CRAN](http://www.r-pkg.org/badges/version/RLumModel)](http://cran.rstudio.com/package=RLumModel)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/RLumModel)](http://www.r-pkg.org/pkg/RLumModel)
[![Downloads](http://cranlogs.r-pkg.org/badges/RLumModel)](http://www.r-pkg.org/pkg/RLumModel)
[![Downloads](http://cranlogs.r-pkg.org/badges/last-week/RLumModel)](http://www.r-pkg.org/pkg/RLumModel)
[![Downloads](http://cranlogs.r-pkg.org/badges/last-day/RLumModel)](http://www.r-pkg.org/pkg/RLumModel)

[![Build Status](https://travis-ci.org/R-Lum/RLumModel.svg?branch=dev_0.2.0)](https://travis-ci.org/R-Lum/RLumModel)
[![Build status](https://ci.appveyor.com/api/projects/status/42umfq97ifr021mk/branch/dev_0.2.0?svg=true)](https://ci.appveyor.com/project/RLumSK/rlummodel/branch/dev_0.2.0)


The **R** package 'RLumModel' by Johannes Friedrich (University of Bayreuth, Germany), 
Sebastian Kreutzer (IRAMAT-CRP2A, Université Bordeaux Montaigne, France) and Christoph Schmidt 
(University of Bayreuth, Germany)
provides a collection of various R functions modelling luminescence signals in quartz.

For an introduction and further details, visit the [RLumModel homepage](http://model.r-luminescence.de).

## Installation

#### i. Requirements

Depending on your OS please download and install one of the following:

**Windows (32/64bit)** - 'Rtools' (provided by CRAN)

   http://cran.r-project.org/bin/windows/Rtools/

**Mac OS X** - 'Xcode' (provided by Apple)

   https://developer.apple.com/xcode/downloads/

For **Linux** users *gcc* often comes pre-installed in most distributions. Should *gcc* be not available, however, we kindly refer to the exhaustive collection of installation guides depending on the linux distribution.

#### ii. Install the package

To install the stable version from CRAN, simply run the following from an R console:

```r
install.packages("RLumModel")
```

To install the latest development builds directly from GitHub, run

```r
if(!require("devtools"))
  install.packages("devtools")
devtools::install_github("R-Lum/RLumModel@master")
```

To install a developer build other than 'master', replace the term 'master' in the codeline by the name
of the wanted developer build. 


## Note

**The package comes without any guarantee!**

Please further note that this version is a development version and may change day by day. 
For stable branches please visit the package on CRAN.

## License

The RLumModel package is licensed under the GPLv3. See these files in the main directory for additional details: 

- LICENSE - RLumModel package license (GPLv3)
