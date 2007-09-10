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
For the moment, only the ''exact'' Quine-McCluskey algorithm for crisp sets is implemented.
However it is widely known that, for large number of conditions (over 15), the algorithm is
memory hungry therefore very slow. 
Starting with version 0.4-5 the package has a new experimental function called shortqca() 
that finds exact solutions with a substantially lower memory consumption, therefore much
faster. The next versions will have more functions to address multi-valued and fuzzy-set QCA.
The package doesn't currently handle missing values in the data, therefore it is not yet
possible to perform simplifying assumptions.
As of version 0.1-3 there is also a graphical user interface in the package QCAGUI.
}

\details{
    \tabular{ll}{
    Package: \tab QCA\cr
    Type: \tab Package\cr
    Version: \tab 0.4-5\cr
    Date: \tab 2007-09-10\cr
    License: \tab GPL version 2 or newer\cr
    }
}

\author{
\tabular{ll}{
Author: \tab Adrian Dusa <adi@sas.unibuc.ro>\cr
Maintainer: \tab Adrian Dusa <adi@sas.unibuc.ro>
}}

\references{
Ragin, Charles C. 1987 \emph{The Comparative Method. Moving beyond qualitative and 
quantitative strategies}, Berkeley:University of California Press

Ragin, Charles C. 2000 \emph{Fuzzy-set social science}, Chicago:The University of Chicago Press}
}

\keyword{cluster}

