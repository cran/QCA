\name{findSubsets}

\alias{findSubsets}

\title{Find all possible subsets of a given prime implicant}

\description{
If a prime implicant can be considered a superset of an initial combination of causal
conditions, the reverse is also true: the initial combination is a subset of a prime implicant.
Even more, a less minimum prime implicant (with more literals) is also a subset of another
shorter (more minimum) prime implicant.
This function finds all possible such subsets for a given prime implicant.
}

\usage{
findSubsets(noflevels, row.no, maximum)
}

\arguments{
  \item{noflevels}{a vector containing the number of levels for each causal condition}
  \item{row.no}{the row number where the (minimum) prime implicant is located}
  \item{maximum}{the maximum line number (subset) to be returned}
}

\value{
a vector with the line numbers of all possible subsets
}

\references{
Dusa, Adrian 2007 \emph{A mathematical approach to the boolean minimization problem}, http://www.compasss.org/WPShort.htm

Dusa, Adrian 2007 \emph{Enhancing Quine-McCluskey}, http://www.compasss.org/WPShort.htm
}

\author{
Adrian Dusa\cr
Romanian Social Data Archive\cr
adi@roda.ro\cr
Faculty of Sociology and Social Work, University of Bucharest\cr
adi@sas.unibuc.ro
}

\seealso{'findPrimes'}

\examples{
# all three conditions are binary, having two levels: 0 and 1
noflevels <- c(2, 2, 2)

findSubsets(noflevels, 2)     #  5  8 11 14 17 20 23 26

# stopping at maximum row number 20
findSubsets(noflevels, 2, 20) #  5  8 11 14 17 20
}

\keyword{misc}
