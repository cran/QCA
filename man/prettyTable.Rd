\name{prettyTable}

\alias{prettyTable}

\title{Print a nicely formatted matrix or truth table}

\description{
R prints the content of a matrix with the values alligned to the right. This function computes
the number of spaces needed to reach the middle of the column (minus one for odd number of
characters) and adds a certain number of spaces to each value from the table in order to alling
these values to the middle.
}

\usage{
prettyTable(mytable)
}

\arguments{
  \item{mytable}{a matrix}
}

\value{
a matrix with character elements
}

\author{
Adrian Dusa\cr
Romanian Social Data Archive\cr
adi@roda.ro\cr
Faculy of Sociology and Social Work, University of Bucharest\cr
adi@sas.unibuc.ro
}

\seealso{'print'}

\examples{
mymatrix <- matrix(sample(0:1, 20, replace=TRUE), nrow=5)
colnames(mymatrix) <- c("First", "Second", "Third", "Fourth")
rownames(mymatrix) <- c("One", "Two", "Three", "Four", "Five")
print(prettyTable(mymatrix))


}

\keyword{misc}
