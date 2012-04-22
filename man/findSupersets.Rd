\name{findSupersets}

\alias{findSupersets}

\title{Find all Possible Supersets of One or Several Combinations}

\description{
It is a general rule that all supersets can be found in the \eqn{n^k} space,
understood as all possible combinations of values in any combination of bases \eqn{n},
each causal condition having three or more levels (Dusa, 2010, 2007).
There is a finite number of prime implicants for any combination of causal conditions, equal to
\eqn{2^k - 1}, where \eqn{k} is the number of causal conditions. Counting out the input combination
itself, the actual number is \eqn{2^k - 2}.
This function computes all possible unique supersets for a specific set of combinations
(either to explain or to exclude).
}

\usage{
findSupersets(noflevels, input.combs)
}

\arguments{
  \item{noflevels}{A vector containing the number of levels for each causal condition plus 1
  (because all prime implicants are to be found in the higher matrix)}
  \item{input.combs}{A matrix with combinations of causal conditions or a vector of
  line numbers from the same matrix}
}

\value{
a vector with the line numbers of all possible prime implicants in the \eqn{3^k} space
}

\references{
A. Dusa. \emph{Enhancing Quine-McCluskey}. WP 2007-49, COMPASSS, 2007.\cr
URL: http://www.compasss.org/files/WPfiles/Dusa2007a.pdf.

A. Dusa. A Mathematical Approach to the Boolean Minimization Problem.
\emph{Quality & Quantity}, 44(1): 99-113, 2010.
}

\seealso{\code{\link{getRow}}, \code{\link{findSubsets}}}

\examples{
# all three conditions are binary, having two levels: 0 and 1
noflevels <- c(2, 2, 2)

# 2 4 5 10 11 13 14
findSupersets(noflevels + 1, 14)

# 2 7 8 10 11 16 17
findSupersets(noflevels + 1, 17)

# both line numbers 14 and 17
# 2 4 5 7 8 10 11 13 14 16 17
findSupersets(noflevels + 1, c(14, 17))


# input.combs as a matrix
(input.combs <- getRow(noflevels + 1, c(14, 17)))

#     [,1] [,2] [,3]
#[1,]    1    1    1
#[2,]    1    2    1

# 2 4 5 7 8 10 11 13 14 16 17
findSupersets(noflevels + 1, input.combs)
}

\keyword{functions}
