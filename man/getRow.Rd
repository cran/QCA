\name{getRow}

\alias{getRow}

\title{Get a Specific Row from Configuration and Implicant Matrices}

\description{
This function transforms the decimal representation of a row number into its corresponding combination 
of levels in configuration matrices or into its corresponding combination of level index values in implicant
matrices.
}

\usage{
getRow(noflevels, row.no, zerobased = FALSE)
}

\arguments{
  \item{noflevels}{A vector containing the number of levels for each condition}
  \item{row.no}{A vector, the desired row numbers}
  \item{zerobased}{Logical, the first row number is zero}
}

\details{
A configuration matrix is formed by all unique combinations of condition levels and represents an 
essential part of any truth table. For any number of levels, the number of configurations is given 
by \eqn{\prod_{j = 1}^{k}{p_j}}, where \eqn{p_j} is the number of levels for condition \eqn{j} and 
\eqn{k} is the number of conditions. 

The implicant matrix consists of all supersets of the rows of a configuration matrix, including the 
configurations themselves as well as the empty set. The formula for the number of implicants is 
\eqn{\prod_{j = 1}^{k}{p_j + 1}}.

The \code{getRow()} function transforms the decimal representation of a row number from a
configuration matrix into its corresponding combination of levels or from an implicant matrix 
into its corresponding combination of level index values.
}

\value{
a matrix with the combination of levels / level index values corresponding to the desired row 
number(s)
}

\seealso{\code{\link{createMatrix}}, \code{\link[base]{expand.grid}}}

\examples{
# all three conditions have two levels
x <- c(2, 2, 2)

# rows from the configuration matrix
# 1 1 1
getRow(noflevels = x, row.no = 8)

# 0 0 0
getRow(noflevels = x, row.no = 0, zerobased = TRUE)

# same as above 
getRow(noflevels = x, row.no = 1)

# second condition has three levels: 0, 1 and 2
x <- c(2, 3, 2)

# 1 2 0
getRow(noflevels = x, row.no = 11)

# the empty set in the implicant matrix 
getRow(noflevels = x, 1) - 1
}

\keyword{functions}
