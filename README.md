# QCA

[![Build Status](https://travis-ci.org/rstudio/shiny.svg?branch=master)](https://travis-ci.org/dusadrian/QCA)


QCA is a methodology that bridges the qualitative and quantitative divide
in social science research. It uses a boolean algorithm that results in a minimal
causal combination which explains a given phenomenon. It handles all types of analyses:
csQCA, mvQCA, fsQCA, temporal QCA and even CNA.

Package QCA acts as an aggregator for various QCA related packages, and since version 2.0
it is further developed in tandem with package QCAGUI.


## Installation

To install the stable version from CRAN, simply run the following from an R console:

```r
install.packages("QCA")
```

To install the latest development builds directly from GitHub, run this instead:

```r
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("dusadrian/QCA")
```


## License

The package QCA is licensed under the GPLv3, with additional licences for external libraries
(see files in the inst directory for additional details)
