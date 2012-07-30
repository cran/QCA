\name{createMatrix}

\alias{createMatrix}

\title{Create Configuration and Implicant Matrices}

\description{
This function creates configuration and implicant matrices. Note that it is mainly used internally. Proper truth tables 
should be created with \code{truthTable}. Proper implicant matrices should be created with \code{allExpressions}.
}

\usage{
createMatrix(noflevels, logical = FALSE)
}

\arguments{
  \item{noflevels}{The number of levels for each condition}
  \item{logical}{Logical, return the matrix in logical values (only binary-level crisp data)}
}

\details{
Configuration matrices contain all unique Boolean products of \eqn{k} literals that can be formed from \eqn{k} conditions. 
The total number of configurations is given by the product of the number of condition levels. \code{createMatrix()} should 
be among the fastest functions to create truth table structures, optimising ideas inspired from the base R function 
\code{expand.grid()}.\cr

The implicant matrix consists of all configurations and their supersets, including the empty set. The formula for the number 
of implicants is \eqn{\prod_{j = 1}^{k}{p_j + 1}}, where \eqn{p_j} is the number of levels for condition \eqn{j} and \eqn{k} 
is the number of conditions.
}

\references{
A. Dusa. A Mathematical Approach to the Boolean Minimization Problem.
\emph{Quality & Quantity}, 44(1):99-113, 2010.

A. Dusa. \emph{Enhancing Quine-McCluskey}. WP 2007-49, COMPASSS, 2007.\cr
URL: http://www.compasss.org/wpseries/Dusa2007b.pdf.

C. C. Ragin. \emph{The Comparative Method: Moving beyond Qualitative and Quantitative Strategies}. 
University of California Press, Berkeley, 1987.
}

\seealso{\code{\link{allExpressions} \link{truthTable}}}

\examples{
# create a configuration matrix with 3 binary-level conditions
noflevels <- rep(2, 3)
createMatrix(noflevels)

# its implicant matrix, where "-1" stands for an eliminated condition
createMatrix(noflevels + 1) - 1

# create a configuration matrix, where the second condition has three levels 
noflevels <- c(2, 3, 2)
createMatrix(noflevels)

# its implicants matrix
createMatrix(noflevels + 1) - 1
}

\keyword{functions}
