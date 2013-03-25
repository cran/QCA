\name{findSupersets}

\alias{findSupersets}

\title{Find Supersets of Configurations in the Implicant Matrix}

\description{
This function finds all possible unique supersets for a specific set of configurations 
in the implicant matrix.
}

\usage{
findSupersets(noflevels3k, input.combs)
}

\arguments{
  \item{noflevels3k}{A vector containing the number of levels for each condition incremented by 1.}
  \item{input.combs}{A matrix of configurations or a vector of their row numbers in the implicant matrix.}
}

\details{
The number of supersets for any combination of condition literals is equal to
\eqn{2^k - 1}, where \eqn{k} is the number of conditions. The implicant matrix 
consists of all configurations and their supersets, including the empty set. The formula 
for the number of implicants is \eqn{\prod_{j = 1}^{k}{p_j + 1}}, where \eqn{p_j} is 
the number of levels for condition \eqn{j} and \eqn{k} is the number of conditions 
(Dusa, 2010, 2007). The \code{findSupersets()} function finds all unique supersets of 
a specific set of configurations in the implicant matrix.
}

\value{
A vector of the line numbers of all possible supersets in the implicant matrix.
}

\references{
A. Dusa. \emph{Enhancing Quine-McCluskey}. WP 2007-49, COMPASSS, 2007.\cr
URL: http://www.compasss.org/wpseries/Dusa2007b.pdf.

A. Dusa. A Mathematical Approach to the Boolean Minimization Problem.
\emph{Quality & Quantity}, 44(1):99-113, 2010.
}

\seealso{\code{\link{getRow}}, \code{\link{findSubsets}}}

\examples{
# three conditions with two levels each
noflevels <- c(2, 2, 2)

# row number 14 of the implicant matrix corresponding to the configuration
# containing the first level of all condition
getRow(noflevels = noflevels + 1, row.no = 14) - 1

# the row numbers of the implicant matrix which correspond to supersets of this
# configuration
(rows <- findSupersets(noflevels + 1, input.combs = 14))

# the corresponding matrix representation of these rows 
getRow(noflevels = noflevels + 1, row.no = rows) - 1

# now for row numbers 14 and 17
getRow(noflevels = noflevels + 1, row.no = c(14, 17)) - 1
(rows <- findSupersets(noflevels + 1, input.combs = c(14, 17)))
getRow(noflevels = noflevels + 1, row.no = rows) - 1

# in matrix representation
(input.combs <- getRow(noflevels + 1, row.no = c(14, 17)))
findSupersets(noflevels + 1, input.combs = input.combs)

}

\keyword{functions}
