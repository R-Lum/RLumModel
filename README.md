# RLumModel

The **R** package 'RLumModel' by the Johannes Friedrich (University of Bayreuth, Germany), 
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

Please further note that this version is a development version and may change day by day. For stable branches please visit
the package on CRAN ... not yet.

## License

The Luminescence package is licensed under the GPLv3. See these files in the main directory for additional details: 

- LICENSE - Luminescence package license (GPLv3)
