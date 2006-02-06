\name{all.expr}

\alias{all.expr}

\title{Create a matrix with all possible combinations of conditions}

\description{
There are \eqn{3^k-1} possible expressions (where k is the number of conditions)
for a crisp-set procedure. This function creates a matrix by row-binding the truth tables
for all expressions.
}

\usage{
all.expr(column.names, inside=FALSE)
}

\arguments{
  \item{column.names}{a vector of strings, the column names of the dataset}
  \item{inside}{if not inside the qmcc function, print the result matrix without NAs}
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
all.expr(column.names)


}

\keyword{misc}
