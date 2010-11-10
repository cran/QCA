\name{QCA-package}

\alias{QCA-package}

\alias{QCA}

\docType{package}

\title{
Qualitative Comparative Analysis
}

\description{
This package implements the comparative methodology as described in Ragin (1987) and Ragin (2000).
The analysis is the bridge between the qualitative and quantitative worlds, making use of the
qualitative procedures in a systematic, algorithmic way (therefore increasing the "confidence"
in the results, as understood by quantitative researchers).
The ''exact'' Quine-McCluskey algorithm for crisp sets is implemented, both for bvQCA (binary-value QCA)
and for mvQCA (multi-value QCA, from version 0.6-0).
Starting with version 0.4-5 the package has a new function called "eqmcc" (from "E"nhanced
Quine-McCluskey) that finds exact solutions with a substantially lower memory consumption.
The next versions of the package will have more functions to address fsQCA (fuzzy-sets QCA),
tsQCA (temporal-sequence QCA) and to deal with missing values in the data.
There is also a graphical user interface in the package QCAGUI.
}

\details{
    \tabular{ll}{
    Package: \tab QCA\cr
    Type: \tab Package\cr
    Version: \tab 0.6-5\cr
    Date: \tab 2010-11-10\cr
    License: \tab GPL (>= 2)\cr
    }
}

\author{
\tabular{ll}{
Author: \tab Adrian Dusa <dusadrian@unibuc.ro>\cr
Maintainer: \tab Adrian Dusa <dusadrian@unibuc.ro>
}}

\references{
Ragin, Charles C. (1987) \emph{The Comparative Method. Moving beyond qualitative and
quantitative strategies}, Berkeley:University of California Press

Dusa, Adrian (2010) \emph{A mathematical approach to the boolean minimization problem},
Quality & Quantity vol.44, no.1, pp.99-113

Dusa, Adrian (2007a) \emph{Enhancing Quine-McCluskey}, http://www.compasss.org/files/WPfiles/Dusa2007a.pdf
}

\keyword{cluster}

