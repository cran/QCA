\name{QCA-package}

\alias{QCA-package}

\alias{QCA}

\docType{package}

\title{
Qualitative Comparative Analysis
}

\description{
This package implements the method of Qualitative Comparative Analysis (QCA) as developed by Ragin (1987, 2000, 2008).
QCA is a configurational comparative technique which uses Boolean instead of linear algebra. This difference changes 
the understanding of causation as symmetric (linear algebra) to an understanding of causation as asymmetric (Boolean algebra). 

QCA is based on set relations between a number of (explaining) conditions and an (explained) outcome, with the two 
concepts of necessity and sufficiency at its core. If some condition or combination of conditions is a superset of 
the outcome, the former is necessary for the latter. If some condition or combination of conditions is a subset of
the outcome, the former is sufficient for the latter.

Three related variants of QCA exist: crisp-set QCA (csQCA), multi-value QCA (mvQCA) and fuzzy-set QCA (fsQCA). A 
subvariant of csQCA called temporal QCA (tQCA) is suitable for event sequence relations.  

In this package, the "exact" Quine-McCluskey algorithm for crisp sets is implemented for csQCA and mvQCA (from 
version 0.6-0), and fsQCA as well as tQCA from version 1.0-0. Starting with version 0.4-5 the package has a new function 
called "eqmcc" ("e"nhanced Quine-McCluskey) which finds exact solutions faster and with substantially lower 
memory consumption (Dusa 2007, 2010).

How to make the most of R's and the QCA package's capabilities for QCA is demonstrated in detail with many examples 
by Thiem and Dusa (2012). 

}

\details{
    \tabular{ll}{
    Package: \tab QCA\cr
    Type:    \tab Package\cr
    Version: \tab 1.0-2\cr
    Date:    \tab 2012-04-22\cr
    License: \tab GPL (>= 2)\cr
    }
}

\author{

\bold{Authors}:\cr
Adrian Dusa\cr
Department of Sociology\cr
University of Bucharest\cr
\email{dusadrian@unibuc.ro}

Alrik Thiem\cr
Department of Humanities, Social and Political Sciences\cr
Swiss Federal Institute of Technology Zurich\cr
\email{thiem@sipo.gess.ethz.ch}

\bold{Maintainer}:\cr
Adrian Dusa
}

\references{
A. Dusa. \emph{Enhancing Quine-McCluskey}. WP 2007-49, COMPASSS, 2007.\cr
URL: http://www.compasss.org/files/WPfiles/Dusa2007a.pdf.

A. Dusa. A Mathematical Approach to the Boolean Minimization Problem.
\emph{Quality & Quantity}, 44(1): 99-113, 2010.

C. C. Ragin. \emph{The Comparative Method: Moving beyond Qualitative and Quantitative Strategies}. 
University of California Press, Berkeley, 1987.

C. C. Ragin. \emph{Fuzzy-Set Social Science}, University of Chicago Press, Chicago, 2000.

C. C. Ragin. \emph{Redesigning Social Inquiry: Fuzzy Sets and Beyond}. University of Chicago Press, 
Chicago, 2008.

A. Thiem and A. Dusa. \emph{Qualitative Comparative Analysis with R: A User's Guide}. 
Springer, New York, 2012.

}

\keyword{package}

