\name{QCA-package}

\alias{QCA-package}

\alias{QCA}

\docType{package}

\title{
Qualitative Comparative Analysis
}

\description{
This library implements the comparative methodology as described in Ragin (1987) and Ragin (2000).
The analysis is the bridge between the qualitative and quantitative worlds, making use of the
qualitative procedures in a systematic, algorithmic way (therefore increasing the "confidence"
in the results, as understood by quantitative researchers).
The ''exact'' Quine-McCluskey algorithm for crisp sets is implemented, both for bvQCA (binary-value QCA)
and for mvQCA (multi-value QCA).
Starting with version 0.4-5 the package has a new function called "eqmcc" (from "E"nhanced
Quine-McCluskey) that finds exact solutions with a substantially lower memory consumption,
and from version 0.6-0 this function also performs mvQCA.
The next versions of the package will have more functions to address fsQCA (fuzzy-sets QCA) and to
deal with missing values in the data.
As of version 0.1-3 there is also a graphical user interface in the package QCAGUI.
}

\details{
    \tabular{ll}{
    Package: \tab QCA\cr
    Type: \tab Package\cr
    Version: \tab 0.6-3\cr
    Date: \tab 2009-10-09\cr
    License: \tab GPL version 2 or newer\cr
    }
}

\author{
\tabular{ll}{
Author: \tab Adrian Dusa <dusa.adrian@unibuc.ro>\cr
Maintainer: \tab Adrian Dusa <dusa.adrian@gmail.com>
}}

\references{
Ragin, Charles C. (1987) \emph{The Comparative Method. Moving beyond qualitative and
quantitative strategies}, Berkeley:University of California Press

Dusa, Adrian (2007) \emph{A mathematical solution to the boolean minimization problem}, available on
http://www.compasss.org/files/WPfiles/Dusa2007.pdf

Dusa, Adrian (2007a) \emph{Enhancing Quine-McCluskey}, http://www.compasss.org/files/WPfiles/Dusa2007a.pdf
}

\keyword{cluster}

