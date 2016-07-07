\name{QCA-package}

\alias{QCA-package}

\alias{QCA}

\docType{package}

\title{
Core functions to perform Qualitative Comparative Analysis.
}

\description{
The package \pkg{QCA} contains the inner, core functions needed to perform Qualitative
Comparative Analysis in R, using the tandem \pkg{QCA} / \pkg{QCAGUI} packages. Both
packages are needed (they depend on each other) to have a complete functionality.
Loading package \pkg{QCA} will automatically load package \pkg{QCAGUI}, and the later
(if loaded separately) will load package \pkg{QCA} when first using a function.

It holds the C code for the minimization procedure(s) and for creating the truth tables,
plus many other (internal) functions useful to format the results to interpretable
versions: sorting QCA type matrices and vectors, writing prime implicants and overall
solutions are just a few examples of inner level functionality the package needs in
order to produce the QCA solutions.

All user level functions, including the Graphical User Interface are visible in the
\pkg{QCAGUI} package.
}

\details{
    \tabular{ll}{
    Package: \tab QCA\cr
    Type:    \tab Package\cr
    Version: \tab 2.4\cr
    Date:    \tab 2016-07-07\cr
    License: \tab GPL (>= 2)\cr
    }
}

\author{

\bold{Authors}:\cr
Adrian Dusa\cr
Department of Sociology\cr
University of Bucharest\cr
\email{dusa.adrian@unibuc.ro}

Alrik Thiem\cr
(from version 1.0-0, up to version 1.1-3)\cr
Department of Philosophy\cr
University of Geneva\cr
\email{alrik.thiem@unige.ch}

\bold{Maintainer}:\cr
Adrian Dusa
}


\keyword{package}

