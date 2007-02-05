\name{createMatrix}

\alias{createMatrix}

\title{Create a base matrix for the truth table}

\description{
The truth table consists from all combinations of presence/absence of conditions (coded binary 1/0).
There are \eqn{2^k} such combinations and this function should be among the fastests to create this
matrix, using ideas inspired from expand.grid
}

\usage{
createMatrix(noflevels)
}

\arguments{
  \item{noflevels}{a vector containing the number of levels for each variable in the dataset}
}

\references{Ragin, Charles C. 1987 \emph{The Comparative Method. Moving beyond qualitative 
and quantitative strategies}, Berkeley: University of California Press}

\author{
Adrian Dusa\cr
Romanian Social Data Archive, University of Bucharest\cr
adi@roda.ro
}

\seealso{'truthTable'}

\examples{
# create a binary matrix based on 3 conditions
createMatrix(rep(2, 3))

# create a matrix based on 3 conditions where the second has three levels
createMatrix(c(2, 3, 2))

# the matrix has 2*3*2 = 12 rows
}

\keyword{misc}
