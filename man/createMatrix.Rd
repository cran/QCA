\name{createMatrix}

\alias{createMatrix}

\title{Create the prod(k) matrix}

\description{
The \code{prod(k + 1)} matrix is the generalisation of the \eqn{3^k} matrix, for any combination of binary and multi-value
crisp sets, where \eqn{k} is a vector with the number of levels for each causal condition.\cr
For example, the truth table consists of all combinations of the presence and absence of conditions (coded binary 1/0).
There are \eqn{2^k} such combinations and \code{createMatrix()} should be among the fastest to create this
matrix, optimising ideas inspired from the base R function \code{expand.grid()}.\cr
The \code{prod(k + 1)} matrix has exactly the same structure as the \eqn{3^k} matrix, given that, for instance, \eqn{3^3} = 3*3*3,
which is simply the same as \code{prod(rep(3, 3))}. It is easy to observe that \eqn{3^k} is just a special case of \code{prod(rep(3, k))}.\cr
For multi-values, the structure \eqn{3^k} cannot hold, as each causal combination can have different number of levels. This issue
is easily solved by using \code{createMatrix()} instead.
}

\usage{
createMatrix(noflevels, logical = FALSE)
}

\arguments{
  \item{noflevels}{A vector containing the number of levels for each variable in the dataset}
  \item{logical}{Logical, return the matrix in logical values (only binary-value crisp data)}
}

\references{
A. Dusa. A Mathematical Approach to the Boolean Minimization Problem.
\emph{Quality & Quantity}, 44(1), pp.99-113, 2010.

A. Dusa. \emph{Enhancing Quine-McCluskey}. WP 2007-49, COMPASSS, 2007.\cr
URL: http://www.compasss. org/files/WPfiles/Dusa2007a.pdf.

C. C. Ragin. \emph{The Comparative Method: Moving beyond Qualitative and Quantitative Strategies}. 
University of California Press, Berkeley, 1987.
}

\seealso{\code{\link{allExpressions} \link{truthTable}}}

\examples{
# create a binary 2^k matrix based on 3 conditions, essentially the truth table
noflevels <- rep(2, 3)
createMatrix(noflevels)

# its 3^k matrix, where all the prime implicants are, where "-1" means a minimised literal
createMatrix(noflevels + 1) - 1

# this is essentially the same structure as the more familiar
# (N.B. there is also a "raw" argument)
allExpressions(noflevels)

# or even more familiar, with all possible PIs arranged
allExpressions(noflevels, arrange = TRUE)

# create a truth table matrix based on 3 conditions where the second has three levels
# the matrix has 2*3*2 = prod(c(2, 3, 2)) = 12 rows
noflevels <- c(2, 3, 2)
createMatrix(noflevels)

# its prod(k) matrix, the generalised version of the 3^k matrix
# which has prod(noflevels + 1) rows
createMatrix(noflevels + 1) - 1

}

\keyword{functions}
