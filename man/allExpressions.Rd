\name{allExpressions}

\alias{allExpressions}

\title{Create a Matrix with all Possible Combinations of Conditions}

\description{
There are exactly \eqn{3^k} possible expressions (where \eqn{k} is the number of conditions) for a binary 
crisp-set procedure. For multi-value crisp data, the general formula for the number of possible expressions 
is \code{prod(noflevels + 1)}, where \code{noflevels} is a vector with the number of levels for each causal condition.
As the matrix grows exponentially, it is not recommended to run this function for a large number of conditions (> 15).
}

\usage{
allExpressions(noflevels, raw = FALSE, arrange = FALSE)
}

\arguments{
  \item{noflevels}{The number of levels for each causal condition}
  \item{raw}{Logical, return the raw result matrix; if FALSE, print the result matrix without NAs}
  \item{arrange}{Logical, try to arrange the result matrix for visual inspection
   (takes a lot of additional time for large matrices)}
}

\value{
A matrix with \code{prod(noflevels + 1)} rows
}

\references{
C. C. Ragin. \emph{Fuzzy-Set Social Science}, University of Chicago Press, Chicago, 2000.
}

\seealso{\code{\link{createMatrix}}}


\examples{
# for 3 conditions
allExpressions(rep(3, 3))

# the same matrix, this time arranged better
allExpressions(rep(3, 3), arrange = TRUE)
}

\keyword{functions}
