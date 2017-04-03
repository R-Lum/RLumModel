# RLumModel

The **R** package 'RLumModel' by Johannes Friedrich (University of Bayreuth, Germany), 
Sebastian Kreutzer (IRAMAT-CRP2A, Université Bordeaux Montaigne, France) and Christoph Schmidt 
(University of Bayreuth, Germany)
provides a collection of various R functions modelling luminescence signals in quartz.

For an introduction and further details, visit the [RLumModel homepage](http://model.r-luminescence.de).

## Package metrics 

### CRAN statistics
[![CRAN](http://www.r-pkg.org/badges/version/RLumModel)](http://cran.rstudio.com/package=RLumModel)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/RLumModel)](http://www.r-pkg.org/pkg/RLumModel)
[![Downloads](http://cranlogs.r-pkg.org/badges/RLumModel)](http://www.r-pkg.org/pkg/RLumModel)
[![Downloads](http://cranlogs.r-pkg.org/badges/last-week/RLumModel)](http://www.r-pkg.org/pkg/RLumModel)
[![Downloads](http://cranlogs.r-pkg.org/badges/last-day/RLumModel)](http://www.r-pkg.org/pkg/RLumModel)

### Impact metrics
[![Research software impact](http://depsy.org/api/package/cran/RLumModel/badge.svg)](http://depsy.org/package/r/RLumModel)
[![Github Stars](https://img.shields.io/github/stars/R-Lum/RLumModel.svg?style=social&label=Github)](https://github.com/R-Lum/RLumModel)

### Platform tests status
[![Build Status](https://travis-ci.org/R-Lum/RLumModel.svg?branch=dev_0.2.0)](https://travis-ci.org/R-Lum/RLumModel)
[![Build status](https://ci.appveyor.com/api/projects/status/42umfq97ifr021mk/branch/dev_0.2.0?svg=true)](https://ci.appveyor.com/project/RLumSK/rlummodel/branch/dev_0.2.0)

### Unit test status
[![Coverage Status](https://img.shields.io/codecov/c/github/R-Lum/RLumModel.svg)](https://codecov.io/github/R-Lum/RLumModel?branch=dev_0.2.0)

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

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the [GNU General Public License](https://github.com/R-Lum/RLumModel/blob/master/LICENSE) for more details.

## Related projects 

* [Luminescence](https://github.com/R-Lum/Luminescence)

