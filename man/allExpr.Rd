\name{allExpr}

\alias{allExpr}

\title{Create a matrix with all possible combinations of conditions}

\description{
There are \eqn{3^k-1} possible expressions (where k is the number of conditions)
for a crisp-set procedure. This function creates a matrix by row-binding the truth tables
for all expressions.
}

\usage{
allExpr(column.names, inside=FALSE)
}

\arguments{
  \item{column.names}{a vector of strings, the column names of the dataset}
  \item{inside}{if TRUE, returns the raw result matrix; if FALSE, print the result matrix without NAs}
}

\value{
a matrix with \eqn{3^k-1} rows
}

\references{Ragin, Charles C. 2000 \emph{Fuzzy-set social science},
Chicago: The University of Chicago Press}

\author{
Adrian Dusa\cr
Romanian Social Data Archive, University of Bucharest\cr
adi@roda.ro
}

\examples{
column.names <- c("First", "Second", "Third")
allExpr(column.names)
}

\keyword{misc}
