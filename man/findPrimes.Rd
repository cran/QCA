\name{findPrimes}

\alias{findPrimes}

\title{Find all possible prime implicants of one or several combinations of causal conditions}

\description{
There is a finite number of prime implicants for any combination of causal conditions, equal to
\eqn{2^k - 1}, where \eqn{k} is the number of causal conditions. Counting out the input combination
itself, the actual number is \eqn{2^k - 2}.
This function computes all possible unique prime implicants for a specific set of combinations
(either to explain or to exclude)
}

\usage{
findPrimes(noflevels, input.combs)
}

\arguments{
  \item{noflevels}{a vector containing the number of levels for each causal condition}
  \item{input.combs}{a matrix with combinations of causal conditions or a vector of
  line numbers from the same matrix}
}

\value{
a vector with the line numbers of all possible prime implicants
}

\references{
Dusa, Adrian 2007 \emph{A mathematical approach to the boolean minimization problem},
http://www.compasss.org/WPShort.htm

Dusa, Adrian 2007 \emph{Enhancing Quine-McCluskey}, http://www.compasss.org/WPShort.htm
}

\author{
Adrian Dusa\cr
Romanian Social Data Archive\cr
adi@roda.ro\cr
Faculty of Sociology and Social Work, University of Bucharest\cr
adi@sas.unibuc.ro
}

\seealso{'getRow', 'findSubsets'}

\examples{
# all three conditions are binary, having two levels: 0 and 1
noflevels <- c(2, 2, 2)

findPrimes(noflevels, 14)         #  2  4  5 10 11 13 14
findPrimes(noflevels, 17)         #  2  7  8 10 11 16 17

# both line numbers 14 and 17
findPrimes(noflevels, c(14, 17))  #  2  4  5 10 11 13 14  7  8 16 17


# input.combs as a matrix
(input.combs <- getRow(noflevels + 1, c(14, 17)))

#     [,1] [,2] [,3]
#[1,]    1    1    1
#[2,]    1    2    1

findPrimes(noflevels, input.combs) #  2  4  5 10 11 13 14  7  8 16 17
}

\keyword{misc}
