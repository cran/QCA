\name{allExpressions}

\alias{allExpressions}

\title{Create an Implicant Matrix}

\description{
This function creates an implicant matrix. The implicant matrix consists of all supersets of a 
truth table's configurations, including the configurations themselves as well as the empty set.
}

\usage{
allExpressions(noflevels, raw = FALSE, arrange = FALSE)
}

\arguments{
  \item{noflevels}{The number of levels for each condition}
  \item{raw}{Logical, return the implicant matrix with "don't care" indicator}
  \item{arrange}{Logical, try to arrange the result matrix for visual inspection
                 (takes a lot of additional time for large matrices)}
}

\details{
The formula for the number of implicants is \eqn{\prod_{j = 1}^{k}{p_j + 1}}, where \eqn{p_j} 
is the number of levels for condition \eqn{j} and \eqn{k} is the number of conditions. As the 
matrix grows exponentially, it is not recommended to run this function for a large number of 
conditions (\eqn{k > 15}).
}

\value{
A matrix with \eqn{\prod_{j = 1}^{k}{p_j + 1}} rows
}

\seealso{\code{\link{createMatrix}}}

\examples{
# three conditions with two levels each
allExpressions(noflevels = rep(2, 3))

# three conditions with three levels each
allExpressions(noflevels = rep(3, 3))

# arranged differently
allExpressions(noflevels = rep(3, 3), arrange = TRUE)

# with "don't care" indicator
allExpressions(noflevels = rep(3, 3), raw = TRUE)
}

\keyword{functions}
