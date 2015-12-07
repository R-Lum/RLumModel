# Luminescence

> [![Build status](https://ci.appveyor.com/api/projects/status/jtgqr9a6jajn02y0/branch/dev_0.5.0?svg=true)](https://ci.appveyor.com/project/tzerk/luminescence/branch/dev_0.5.0)
[![Build Status](https://travis-ci.org/R-Lum/Luminescence.svg?branch=dev_0.5.0)](https://travis-ci.org/R-Lum/Luminescence)
**@dev_0.5.0**


> Follow us on [![alt text][1.1]][1] <a href="http://www.twitter.com/RLuminescence" target="_blank">@RLuminescence</a>
[1.1]: http://i.imgur.com/wWzX9uB.png (twitter icon without padding)
[1]: http://www.twitter.com/RLuminescence


The R package 'Luminescence' by the R-Luminescence Group provides a collection of various R functions for luminescence dating data analysis.

For an introduction and further details, visit the [R-Luminescence homepage](http://www.r-luminescence.de).

## Installation

#### i. Requirements

With release of version 0.4.2 part of the *analyse_IRSAR.RF()* function was rewritten in C to increase its perfomance. This inevitably introduced the requirement for the GNU Compiler Collection (*gcc*) when the R package 'Luminescence' is installed from source. Depending on your OS please download and install one of the following:

**Windows (32/64bit)** - 'Rtools' (provided by CRAN)

   http://cran.r-project.org/bin/windows/Rtools/

**Mac OS X** - 'Xcode' (provided by Apple)

   https://developer.apple.com/xcode/downloads/

For **Linux** users *gcc* often comes pre-installed in most distributions. Should *gcc* be not available, however, we kindly refer to the exhaustive collection of installation guides depending on the linux distribution.

#### ii. Install the package

To install the stable version from CRAN, simply run the following from an R console:

```r
install.packages("Luminescence")
```


To install the latest development builds directly from GitHub, run

```r
if(!require("devtools"))
  install.packages("devtools")
devtools::install_github("R-Lum/Luminescence@master")
```

To install a developer build other than 'master', replace the term 'master' in the codeline by the name
of the wanted developer build. 

## Contribute

The R luminescence project is based on and evolves from ideas, contributions and constructive criticism of its users. Help us to maintain and develop the package, to find bugs and create new functions as well as a user-friendly design. Visit our [message board](https://forum.r-luminescence.de) or write us an [e-mail](mailto:team@r-luminescence.de) if anything crosses your mind or if you want your new self-written function to be to implemented. You are kindly invited to bring forward the package with us!

## Note

**The package comes without any guarantee!**

Please further note that this version is a development version and may change day by day. For stable branches please visit
the package on [CRAN 'Luminescence'](http://cran.r-project.org/package=Luminescence).

## License

The Luminescence package is licensed under the GPLv3. See these files in the main directory for additional details: 

- LICENSE - Luminescence package license (GPLv3)
