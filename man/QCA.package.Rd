\name{QCA-package}

\alias{QCA-package}

\alias{QCA}

\docType{package}

\title{
QCA: A Package for Qualitative Comparative Analysis
}

\description{
The package \pkg{QCA} contains functions to perform Qualitative Comparative Analysis,
complemented with a graphical user interface. It implements the comparative method as
first described by Ragin (1987), and extended by Cronqvist and Berg-Schlosser (2009)
and Ragin (2000, 2008). QCA is a bridge between the qualitative and quantitative
research methodologies, making use of the qualitative procedures in a systematic,
algorithmic way (therefore increasing the \dQuote{confidence} in the results, as
understood by quantitative researchers).

The Quine-McCluskey minimization algorithm implemented in this package is
mathematically exact, as described by Dusa (2007), Dusa (2010) and Dusa and Thiem (2015).
It returns the relevant prime implicants for both \dfn{csQCA} (binary crisp sets QCA)
and also for \dfn{mvQCA} (multi-value QCA).

Starting with version 0.4-5 the package \pkg{QCA} gained a new function called
\code{\link{eqmcc}} (from \dfn{"e"nhanced Quine-McCluskey}) that finds exact solutions
with a substantially lower memory consumption. The multi-value extension of the
\code{\link{eqmcc}} function was implemented in version 0.6-0.

Between versions 1.0-0 and up to version 1.1-3, the package welcomed a second co-author
Alrik Thiem, responsible with the manual testing and documentation of functions,
while the main author Adrian Dusa was responsible with all of the software development.
The package gained new functionality by employing \dfn{fsQCA} (fuzzy-set QCA) and also
other types of QCA like \dfn{tsQCA} (temporal QCA), see Caren and Panofsky (2005),
Ragin and Strand (2008). The package is also able to obtain the causal chains similar
to those from the package \bold{\pkg{cna}} (see Ambuehl et all 2015).

The author of this package is grateful for the effort of the former coauthor, Alrik Thiem,
given his responsibilities of testing and documenting the functions. Although the current
documentation is completely re-written, it owes a great deal of impact from the previous
versions.

The results of the \bold{\pkg{QCA}} package are very consistent with (and sometimes even
better than) the results of the other software packages for QCA, most notably
\bold{\pkg{fs/QCA}} by Ragin and Davey (2014) and \bold{\pkg{Tosmana}} by Cronqvist and
Berg-Schlosser (2009). A comparison of several such software is provided by Thiem and
Dusa (2013).

From version 2.0, this package uses a new graphical user interface based on the package
\bold{\pkg{shiny}}. In order to avoid developing different interfaces for different operating
systems, the current GUI was designed to work into a webpage. It uses a combination of HTML,
CSS, jQuery for the user interface, a custom development framework using vector graphics
based on the Raphael library, and extensive Javascript code to bind these altogether. A
first version of this user interface was presented by Dusa (2007), but users should be aware
the current version is much more advanced. Starting with version 2.5, the user interface
gained a web-based command console to offer a complete experience of available functionality.
}

\details{
    \tabular{ll}{
    Package: \tab QCA\cr
    Type:    \tab Package\cr
    Version: \tab 2.6\cr
    Date:    \tab 2017-03-24\cr
    License: \tab GPL (>= 2)\cr
    }
}

\author{

\bold{Authors}:\cr
Adrian Dusa\cr
Department of Sociology\cr
University of Bucharest\cr
\email{dusa.adrian@unibuc.ro}

\bold{Maintainer}:\cr
Adrian Dusa
}

\references{
Ambuehl, M. et all (2015) \emph{A Package for Coincidence Analysis (CNA),
R Package Version 1.0-3 [Computer Program]}\cr
URL: \url{https://cran.r-project.org/package=cna}.

Caren, N.; Panofsky, A. (2005) \dQuote{TQCA: A Technique for Adding 
Temporality to Qualitative Comparative Analysis.} 
\emph{Sociological Methods & Research} vol.34, no.2, pp.147-172.

Cronqvist, L. (2011) \emph{Tosmana: Tool for Small-N Analysis, Version 1.3.2.0 
[Computer Program]}. Trier: University of Trier.

Dusa, A. (2007a) \dQuote{User manual for the QCA(GUI) package in R}.
\emph{Journal of Business Research} vol.60, no.5, pp.576-586.
  
Dusa, A. (2007b) \emph{Enhancing Quine-McCluskey}. WP 2007-49, COMPASSS.\cr
URL: \url{http://www.compasss.org/wpseries/Dusa2007b.pdf}.

Dusa, A. (2010) \dQuote{A Mathematical Approach to the Boolean Minimization 
Problem.} \emph{Quality & Quantity} vol.44, no.1, pp.99-113.

Dusa, A.; Thiem, A. (2015) \dQuote{Enhancing the Minimization of Boolean and
Multivalue Output Functions With eQMC} \emph{Journal of Mathematical Sociology}
vol.39, no.2, pp.92-108.

Ragin, C.C. (1987) \emph{The Comparative Method: Moving beyond Qualitative and 
Quantitative Strategies}. Berkeley: University of California Press.

Ragin, C.C. (2000) \emph{Fuzzy-Set Social Science}. Chicago: University of 
Chicago Press.

Ragin, C.C. (2008) \emph{Redesigning Social Inquiry: Fuzzy Sets and Beyond}. 
Chicago: University of Chicago Press.

Ragin, C.C.; Strand, S.I. (2008) \dQuote{Using Qualitative Comparative
Analysis to Study Causal Order: Comment on Caren and Panofsky (2005)}.
\emph{Sociological Methods & Research} vol.36, no.4, pp.431-441.

Ragin, C.C.; Davey, S. (2014) \emph{fs/QCA: Fuzzy-Set/Qualitative 
Comparative Analysis, Version 2.5 [Computer Program]}. 
Irvine: Department of Sociology, University of California.

Thiem, A.; Dusa, A. (2013) \dQuote{Boolean Minimization in Social 
Science Research: A Review of Current Software for Qualitative Comparative 
Analysis (QCA).} \emph{Social Science Computer Review} vol.31, no.4, pp.505-521.
}


\keyword{package}

