\name{_Cebotari and Vink}

\docType{data}

\alias{CVR}
\alias{CVF}

\title{Ethnic protest in Europe}

\description{
This data set was used by Cebotari and Vink (2013), and it was taken here from the
associated replication file Cebotari and Vink (2015).
}

\usage{
data(CVR)
data(CVF)
}

\format{                  
A data frame containing 29 cases (ethnic minorities) and the following 6 columns:

\tabular{rl}{
\bold{\code{DEMOC}}    \tab Level of democracy \cr
\bold{\code{ETHFRACT}} \tab Degree of ethnic fractionalization \cr 
\bold{\code{GEOCON}}   \tab Territorial concentration \cr 
\bold{\code{POLDIS}}   \tab Political discrimination \cr
\bold{\code{NATPRIDE}} \tab National pride \cr
\bold{\code{PROTEST}}  \tab Ethnopolitical protest \cr
}
}

\details{
There are two different versions of the Cebotari and Vink data: \bold{\code{CVR}} contains
the raw data, and \bold{\code{CVF}} contains the data calibrated to fuzzy-sets.

The causal conditions are:

\bold{\code{DEMOC}} (contextual factor), based on a democracy index ranking countries on a
scale from strong autocracies (0) to strong democracies (10). The fuzzy scores were
calibrated using an exclusion threshold of 2, a crossover of 7 and an inclusion
threshold of 9.5.

\bold{\code{ETHFRACT}} (contextual factor), with raw scores ranging from a perfectly
homogenous society (0) to a highly fragmented country (1). The fuzzy scores were
calibrated using an exclusion threshold of 0, a crossover of 0.495 and an inclusion
threshold of 0.8.

\bold{\code{GEOCON}} (group-related factor) with raw data coded as: widely dispersed (0) and
primarily urban minorities (1) considered territorially dispersed minorities, and
ethnic comunities majoritary in a region (2) and entirely concentrated in one region (3)
considered as territorially concentrated minorities. The fuzzy scores were calibrated
using an exclusion threshold of 0, a crossover of 1.25 and an inclusion threshold of 3.

\bold{\code{POLDIS}} (group-related factor) captures discrimination practices toward minority
groups that vary from no discrimination (0) to exclusive and repressive policies
toward a minority group (4). The fuzzy scores were calibrated using an exclusion
threshold of 0, a crossover of 0.75 and an inclusion threshold of 3. 

\bold{\code{NATPRIDE}} (group-related factor) with raw scores ranging from \sQuote{not at all
proud} (0) to \sQuote{very proud} (3). The fuzzy scores were calibrated using an
exclusion threshold of 0.5, a crossover of 1.5 and an inclusion threshold of 2.5.

The outcome is the column \bold{\code{PROTEST}}, measured on a range from 0 to 5 with
higher values indicating more intense protest actions. The fuzzy scores were
calibrated using an exclusion threshold of 0.5, a crossover of 1.5 and an inclusion
threshold of 3.


}

\references{
Cebotari, V.; Vink, M.P. (2013) \dQuote{A Configurational Analysis of Ethnic
Protest in Europe}. \emph{International Journal of Comparative Sociology}
vol.54, no.4, pp.298-324.

Cebotari, V.; Vink, M.P. (2015) \dQuote{Replication Data for: A configurational
analysis of ethnic protest in Europe}, DOI:
\url{http://dx.doi.org/10.7910/DVN/PT2IB9}, Harvard Dataverse, V2
}

\keyword{datasets}
