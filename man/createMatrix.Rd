\name{createMatrix}

\alias{createMatrix}

\title{Create a base matrix for the truth table}

\description{
The truth table consists from all combinations of presence/absence of conditions, and this (AFAIK)
is the fastest way to create a matrix with these combinations, using ideas inspired from expand.grid
}

\usage{
createMatrix(no.conditions)
}

\arguments{
  \item{no.conditions}{the number of conditions from the dataset}
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
# create a matrix based on 3 conditions
createMatrix(3)

# the matrix has 2^3 = 8 rows
}

\keyword{misc}
