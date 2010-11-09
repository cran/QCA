\name{findSubsets}

\alias{findSubsets}

\title{Find all possible subsets of a given prime implicant}

\description{
It is a general rule that all subsets can be found in the \eqn{n^k} space,
understood as all possible combinations of values in any combination of bases \eqn{n},
each causal condition having three or more levels (Dusa, 2010, 2007).
If a prime implicant can be considered a superset of an initial combination of causal
conditions, the reverse is also true: the initial combination is a subset of a prime implicant.
Even more, a less minimum prime implicant (with more literals) is also a subset of another
shorter (more minimum) prime implicant.
This function finds all possible such subsets for a given prime implicant, in the \eqn{n^k} space.
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
Dusa, Adrian (2010) \emph{A mathematical approach to the boolean minimization problem},
Quality & Quantity vol.44, no.1, pp.99-113

Dusa, Adrian (2007) \emph{Enhancing Quine-McCluskey}, http://www.compasss.org/files/WPfiles/Dusa2007a.pdf
}

\author{
Adrian Dusa\cr
University of Bucharest, Faculty of Sociology and Social Work\cr
dusadrian@unibuc.ro
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
