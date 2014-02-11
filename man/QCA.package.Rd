\name{QCA-package}

\alias{QCA-package}

\alias{QCA}

\docType{package}

\title{
A Package for Qualitative Comparative Analysis
}

\description{
This package implements the method of Qualitative Comparative Analysis (QCA) as 
developed by Ragin (1987) and extended by Ragin (2000, 2008) and Cronqvist and 
Berg-Schlosser (2009). QCA is a family of configurational comparative techniques 
that use Boolean algebra and its various generalizations, fusing concepts of 
set theory and propositional logic.

Three basic variants can currently be processed: crisp-set QCA (csQCA), multi-value 
QCA (mvQCA) and fuzzy-set QCA (fsQCA). A subvariant of csQCA called temporal QCA 
(tQCA) is also available (Caren and Panofsky 2005; Ragin and Strand 2008). The 
'QCA' package extends these basic variants in various directions and offers novel
and complementary functionality.

As of version 0.4-5, the package has had a new function called \code{\link{eqmcc}} 
(enhanced Quine-McCluskey) that finds exact solutions much faster and with 
substantially lower memory consumption than QMC (Dusa 2007, 2010). Since version 
1.0-4, the compilation of major parts of the code in the C language has increased 
the speed of some core functions such as \code{\link{superSubset}}. Since version 
1.1-0, \code{\link{eqmcc}} has been made more flexible: besides the previous 
possibility to relax row dominance (Thiem and Dusa 2013a), QCA path models can now
be identified (equivalent of "causal chains" in Coincidence Analysis 
(Baumgartner 2009a, b)), multivalent outcomes are allowed, and truth tables for 
csQCA and mvQCA can now be coded on the basis of inference-statistical tests 
(currently only exact binomial). For more details, see the package's change log file.

Several data sets from various areas of research have also been integrated in 
order to facilitate familiarization with the package's functionality. Areas 
currently covered are education (\code{\link{d.SS}}), environmental sciences 
(\code{\link{d.Bas}}), evaluation (\code{\link{d.SA}}), health 
(\code{\link{d.BWB}}), management (\code{\link{d.CZH}}), political science 
(\code{\link{d.Emm}}, \code{\link{d.HK}}, \code{\link{d.Kro}}) and sociology 
(\code{\link{d.CS}}). For more details, see the data sets' documentation files.

How to make the most of the package's capabilities is demonstrated in detail by 
Thiem and Dusa (2013b) with examples from political science (note that data sets 
have been renamed). The general reference for the QCA package is Thiem and Dusa 
(2013c). For full publication information, use command \code{citation("QCA")}. 
Later versions of the package than those used in these publications may feature 
different functions, arguments or values, but backward compatibility is ensured.
}

\details{
    \tabular{ll}{
    Package: \tab QCA\cr
    Type:    \tab Package\cr
    Version: \tab 1.1-2\cr
    Date:    \tab 2014-02-11\cr
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
Department of Humanities, Social and Political Sciences\cr
Swiss Federal Institute of Technology Zurich (ETHZ)\cr
\email{thiem@sipo.gess.ethz.ch}

\bold{Maintainer}:\cr
Adrian Dusa
}

\references{
Baumgartner, Michael. 2009a. Uncovering Deterministic Causal Structures: A 
Boolean Approach. 
\emph{Synthese} 170 (1):71-96.

Baumgartner, Michael. 2009b. Inferring Causal Complexity. 
\emph{Sociological Methods & Research} 38 (1):71-101.

Caren, Neal, and Aaron Panofsky. 2005. TQCA: A Technique for Adding Temporality 
to Qualitative Comparative Analysis. 
\emph{Sociological Methods & Research} 34 (2):147-72.

Cronqvist, Lasse, and Dirk Berg-Schlosser. 2009. Multi-Value QCA (mvQCA). In 
\emph{Configurational Comparative Methods: Qualitative Comparative Analysis (QCA) 
and Related Techniques}, eds. B. Rihoux and C. C. Ragin. London: Sage Publications, 
pp. 69-86.

Dusa, Adrian. 2007. \emph{Enhancing Quine-McCluskey}. COMPASSS: WP 2007-49. Available from: 
http://www.compasss.org/wpseries/Dusa2007b.pdf.

Dusa, Adrian. 2010. A Mathematical Approach to the Boolean Minimization Problem. 
\emph{Quality & Quantity} 44 (1):99-113.

Ragin, Charles C. 1987. \emph{The Comparative Method: Moving beyond Qualitative and 
Quantitative Strategies}. Berkeley: University of California Press.

Ragin, Charles C. 2000. \emph{Fuzzy-Set Social Science}. Chicago: University of Chicago Press.

Ragin, Charles C. 2008. \emph{Redesigning Social Inquiry: Fuzzy Sets and Beyond}. 
Chicago: University of Chicago Press.

Ragin, Charles C., and Sarah Ilene Strand. 2008. Using Qualitative Comparative 
Analysis to Study Causal Order: Comment on Caren and Panofsky (2005). 
\emph{Sociological Methods & Research} 36 (4):431-41.

Thiem, Alrik, and Adrian Dusa. 2013. Boolean Minimization in Social Science Research: 
A Review of Current Software for Qualitative Comparative Analysis (QCA). 
\emph{Social Science Computer Review} 31 (4):505-21.

Thiem, Alrik, and Adrian Dusa. 2013b. \emph{Qualitative Comparative Analysis with R: 
A User's Guide}. New York: Springer.

Thiem, Alrik, and Adrian Dusa. 2013c. QCA: A Package for Qualitative Comparative 
Analysis. 
\emph{The R Journal} 5 (1):87-97.
}

\keyword{package}

