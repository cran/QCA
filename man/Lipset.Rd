\name{_Lipset}

\docType{data}

\alias{LR}
\alias{LC}
\alias{LM}
\alias{LF}

\title{Lipset's indicators for the survival of democracy}

\description{
This dataset is taken from Lipset (1959), as used by Rihoux and De Meur (2009),
Cronqvist and Berg-Schlosser (2009) and Ragin (2009).
}

\usage{
data(LR)
data(LC)
data(LM)
data(LF)
}

\format{                  
A data frame containing 18 rows and the following 6 columns:

\tabular{rl}{
\bold{\code{DEV}}  \tab Level of development \cr
\bold{\code{URB}}  \tab Level of urbanization \cr 
\bold{\code{LIT}}  \tab Level of literacy \cr 
\bold{\code{IND}}  \tab Level of industrialization \cr
\bold{\code{STB}}  \tab Government stability \cr
\bold{\code{SURV}} \tab Survival of democracy during the inter-war period. \cr      
}
}

\details{
There are four different versions of the Lipset data:

\tabular{rl}{
\bold{\code{LR}}  \tab contains the raw data \cr
\bold{\code{LC}}  \tab is the same data calibrated to binary crisp sets \cr 
\bold{\code{LM}}  \tab is calibrated to multi-value sets \cr 
\bold{\code{LF}}  \tab is calibrated to fuzzy-sets \cr       
}


The causal conditions are:

\bold{\code{DEV}} referring to the level of development: in the raw data is
the GDP per capita (USD), calibrated in the binary crisp version to 0 if
below 550 USD and 1 otherwise. For the multi-value crisp version, two thresholds
were used: 550 and 850 USD.

\bold{\code{URB}} is the percent of the population in towns with 20,000 or more
inhabitants, calibrated in the crisp versions to 0 if below 50\% and 1 if above.

\bold{\code{LIT}} is the percent of the literate population, calibrated
in the crisp versions to 0 if below 75\% and 1 if above.

\bold{\code{IND}} is the percent of the industrial labor force, calibrated
in the crisp versions to 0 if below 30\% and 1 if above.

\bold{\code{STB}} is a \dQuote{political-institutional} condition added to the
previous four \dQuote{socioeconomic} ones. The raw data has the number of cabinets
which governed in the period under study, calibrated in the crisp versions to 0 if
10 or above and 1 if below 10.

The outcome is the column \bold{\code{SURV}}, calibrated to 0 if negative, and 1 if
positive raw data.


}

\references{
Lipset, S. M. (1959) \dQuote{Some Social Requisites of Democracy: Economic
Development and Political Legitimacy}, \emph{American Political Science Review}
vol.53, pp.69-105.

Cronqvist, L.; Berg-Schlosser, D. (2009) \dQuote{Multi-Value QCA (mvQCA)}, in
Rihoux, B.; Ragin, C. (eds.) \emph{Configurational Comparative Methods. Qualitative
Comparative Analysis (QCA) and Related Techniques}, SAGE.

Rihoux, B.; De Meur, G. (2009) \dQuote{Crisp Sets Qualitative Comparative Analysis
(mvQCA)}, in Rihoux, B.; Ragin, C. (eds.) \emph{Configurational Comparative Methods.
Qualitative Comparative Analysis (QCA) and Related Techniques}, SAGE.

Ragin, C. (2009) \dQuote{Qualitative Comparative Analysis Using Fuzzy-Sets (fsQCA)},
in Rihoux, B.; Ragin, C. (eds.) \emph{Configurational Comparative Methods.
Qualitative Comparative Analysis (QCA) and Related Techniques}, SAGE.
}

\keyword{datasets}
