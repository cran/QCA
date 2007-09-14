\name{allExpressions}

\alias{allExpressions}

\title{Create a matrix with all possible combinations of conditions}

\description{
There are \eqn{3^k-1} possible expressions (where k is the number of conditions)
for a crisp-set procedure. Possible values for conditions are 1 (presence), 0 (absence) and NA
(the condition being a subset of a larger one).
The matrix grows exponentially so it is not reccomended to run this function for large number of 
conditions (say over 12) as the computer will quickly run out of resources.
}

\usage{
allExpressions(no.conditions, inside=TRUE, arrange=FALSE)
}

\arguments{
  \item{no.conditions}{the number of conditions from the dataset}
  \item{inside}{if TRUE, returns the raw result matrix; if FALSE, print the result matrix without NAs}
  \item{arrange}{if TRUE the program tries hard to arrange the result matrix for visual inspection
   (however this takes a lot of additional time for large matrices)}
}

\value{
a matrix with \eqn{3^k-1} rows
}

\references{Ragin, Charles C. 2000 \emph{Fuzzy-set social science},
Chicago: The University of Chicago Press}

\author{
Adrian Dusa\cr
Romanian Social Data Archive\cr
adi@roda.ro\cr
Faculy of Sociology and Social Work, University of Bucharest\cr
adi@sas.unibuc.ro
}

\examples{
# for 3 conditions
allExpressions(3, inside=FALSE)

# the same matrix, this time arranged better
allExpressions(3, inside=FALSE, arrange=TRUE)
}

\keyword{misc}
