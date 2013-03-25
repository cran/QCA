\name{QCA-package}

\alias{QCA-package}

\alias{QCA}

\docType{package}

\title{
Qualitative Comparative Analysis
}

\description{
This package implements the method of Qualitative Comparative Analysis (QCA) as 
developed and extended by Ragin (1987, 2000, 2008). QCA is a configurational 
comparative technique which uses Boolean instead of linear algebra. In particular, 
it draws on the concepts and notation of set theory. 

QCA models involve a number of conditions and outcomes whose subset and superset 
relations are analyzed and framed in terms of necessity and sufficiency. If some 
condition or combination of conditions is a superset of the outcome, the former 
is necessary for the latter. If some condition or combination of conditions is a 
subset of the outcome, the former is sufficient for the latter.

Three related variants of QCA exist: crisp-set QCA (csQCA), multi-value QCA (mvQCA) 
and fuzzy-set QCA (fsQCA). A subvariant of csQCA called temporal QCA (tQCA) is suitable 
for event sequence relations. The QCA package provides extensive functionality for 
all of these variants.  

The ``exact'' Quine-McCluskey algorithm for crisp sets has been implemented for csQCA and mvQCA 
since version 0.6-0, and fsQCA as well as tQCA since version 1.0-0. Starting with version 
0.4-5 the package employed a new function called ``eqmcc'' (enhanced Quine-McCluskey) which finds 
exact solutions much faster and with substantially lower memory consumption (Dusa 2007, 2010).
Since version 1.0-4, computationally intensive parts of the R code were compiled in the C language,
considerably increasing the speed of some functions.

How to make the most of R's and the QCA package's capabilities is demonstrated in detail 
with many examples by Thiem and Dusa (2013). 

}

\details{
    \tabular{ll}{
    Package: \tab QCA\cr
    Type:    \tab Package\cr
    Version: \tab 1.0-5\cr
    Date:    \tab 2013-03-25\cr
    License: \tab GPL (>= 2)\cr
    }
}

\author{

Adrian Dusa\cr
Department of Sociology\cr
University of Bucharest\cr
\email{dusa.adrian@unibuc.ro}

Alrik Thiem
Department of Humanities, Social and Political Sciences\cr
Swiss Federal Institute of Technology Zurich\cr
\email{thiem@sipo.gess.ethz.ch}

\bold{Maintainer}:\cr
Adrian Dusa
}

\references{
A. Dusa. \emph{Enhancing Quine-McCluskey}. WP 2007-49, COMPASSS, 2007.\cr
URL: http://www.compasss.org/wpseries/Dusa2007b.pdf.

A. Dusa. A Mathematical Approach to the Boolean Minimization Problem.
\emph{Quality & Quantity}, 44(1):99-113, 2010.

C. C. Ragin. \emph{The Comparative Method: Moving beyond Qualitative and Quantitative Strategies}. 
University of California Press, Berkeley, 1987.

C. C. Ragin. \emph{Fuzzy-Set Social Science}, University of Chicago Press, Chicago, 2000.

C. C. Ragin. \emph{Redesigning Social Inquiry: Fuzzy Sets and Beyond}. University of Chicago Press, 
Chicago, 2008.

A. Thiem and A. Dusa. \emph{Qualitative Comparative Analysis with R: A User's Guide}. 
Springer, New York, 2012.

}

\keyword{package}

