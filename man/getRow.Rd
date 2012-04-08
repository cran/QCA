\name{getRow}

\alias{getRow}

\title{Get a Specific Row from a Truth Table}

\description{
A truth table is formed by the unique combinations of causal conditions' levels. For binary
crisp sets, there are exactly \eqn{2^k} combinations, where \eqn{k} is the number of conditions.
This function accepts multiple levels (not just 0 an 1)  and its purpose is to transform
the decimal representation of a row number into its corresponding combination of levels.
}

\usage{
getRow(noflevels, row.no, zerobased = FALSE)
}

\arguments{
  \item{noflevels}{A vector containing the number of levels for each condition}
  \item{row.no}{A vector, the desired row number(s) from the truth table}
  \item{zerobased}{Logical, the first row number from the truth table is zero}
}

\value{
a matrix with the combination of levels corresponding to the desired row number(s)
}

\seealso{\code{\link{createMatrix}}, \code{\link[base]{expand.grid}}}

\examples{
# all three conditions are binary, having two levels: 0 and 1
noflevels <- c(2, 2, 2)
getRow(noflevels, 8) # 1 1 1
getRow(noflevels, 0, zerobased=TRUE) # 0 0 0


# the second condition has three levels: 0, 1 and 2
noflevels <- c(2, 3, 2)
getRow(noflevels, 11) # 1 2 0
}

\keyword{functions}
