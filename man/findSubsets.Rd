\name{findSubsets}

\alias{findSubsets}

\title{Find all possible subsets of a given prime implicant}

\description{
It is a general rule that all subsets can be found in the \eqn{3^k} space,
understood as all possible combinations of values in base 3, each variable having
three levels: 0, 1 and 2 (Dusa, 2007, 2007a).
If a prime implicant can be considered a superset of an initial combination of causal
conditions, the reverse is also true: the initial combination is a subset of a prime implicant.
Even more, a less minimum prime implicant (with more literals) is also a subset of another
shorter (more minimum) prime implicant.
This function finds all possible such subsets for a given prime implicant, in the \eqn{3^k} space.
}

\usage{
findSubsets(noflevels, row.no, maximum)
}

\arguments{
  \item{noflevels}{a vector containing the number of levels for each causal condition plus 1
  (because all subsets are to be found in the higher matrix)}
  \item{row.no}{the row number where the (minimum) prime implicant is located}
  \item{maximum}{the maximum line number (subset) to be returned}
}

\value{
a vector with the line numbers of all possible subsets in the \eqn{3^k} space
}

\references{
Dusa, Adrian (2007) \emph{A mathematical approach to the boolean minimization problem}, available on
http://www.compasss.org/files/WPfiles/Dusa2007.pdf

Dusa, Adrian (2007a) \emph{Enhancing Quine-McCluskey}, http://www.compasss.org/files/WPfiles/Dusa2007a.pdf
}

\author{
Adrian Dusa\cr
University of Bucharest, Faculty of Sociology and Social Work\cr
dusa.adrian@unibuc.ro
}

\seealso{\code{\link{findPrimes}}}

\examples{
# all three conditions are binary, having two levels: 0 and 1
noflevels <- c(2, 2, 2)

findSubsets(noflevels + 1, 2)     #  5  8 11 14 17 20 23 26

# stopping at maximum row number 20
findSubsets(noflevels + 1, 2, 20) #  5  8 11 14 17 20
}

\keyword{misc}
