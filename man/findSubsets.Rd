\name{findSubsets}

\alias{findSubsets}

\title{Find all Possible Subsets of a Given Prime Implicant}

\description{
It is a general rule that all subsets can be found in the \eqn{n^k} space,
understood as all possible combinations of values in any combination of bases \eqn{n},
each causal condition having three or more levels (Dusa, 2007, 2010).
If a prime implicant can be considered a superset of an initial combination of causal
conditions, the reverse is also true: the initial combination is a subset of a prime implicant.
Even more, a less minimum prime implicant (with more literals) is also a subset of another
shorter (more minimum) prime implicant.
This function finds all possible such subsets for a given prime implicant, in the \eqn{n^k} space.
}

\usage{
findSubsets(noflevels3k, row.no, maximum)
}

\arguments{
  \item{noflevels3k}{A vector containing the number of levels for each causal condition plus 1
  (because all subsets are to be found in the higher "3k" matrix)}
  \item{row.no}{The row number where the (minimum) prime implicant is located}
  \item{maximum}{The maximum line number (subset) to be returned}
}

\value{
A vector with the line numbers of all possible subsets in the \eqn{prod(n)} space.
}

\references{
A. Dusa. \emph{Enhancing Quine-McCluskey}. WP 2007-49, COMPASSS, 2007.\cr
URL: http://www.compasss.org/files/WPfiles/Dusa2007a.pdf.

A. Dusa. A Mathematical Approach to the Boolean Minimization Problem.
\emph{Quality & Quantity}, 44(1): 99-113, 2010.
}

\seealso{\code{\link{findSupersets}}}

\examples{
# all three conditions are binary, having two levels: 0 and 1
noflevels <- c(2, 2, 2)

findSubsets(noflevels + 1, 2)     #  5  8 11 14 17 20 23 26

# stopping at maximum row number 20
findSubsets(noflevels + 1, 2, 20) #  5  8 11 14 17 20
}

\keyword{functions}
