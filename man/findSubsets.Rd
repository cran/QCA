\name{findSubsets}

\alias{findSubsets}

\title{Find all Possible Subsets of an Implicant}

\description{
This function finds all row numbers in the implicant matrix which correspond to all 
possible subsets of an implicant.
}

\usage{
findSubsets(noflevels3k, row.no, maximum)
}

\arguments{
  \item{noflevels3k}{A vector containing the number of levels for each condition incremented by 1}
  \item{row.no}{The row number of the implicant in the implicant matrix}
  \item{maximum}{The highest row number to be returned}
}

\details{
All subsets of an implicant can be found in the implicant matrix. The implicant matrix 
consists of all configurations and their supersets, including the empty set. The formula 
for the number of implicants is \eqn{\prod_{j = 1}^{k}{p_j + 1}}, where \eqn{p_j} is 
the number of levels for condition \eqn{j} and \eqn{k} is the number of conditions 
(Dusa, 2007, 2010). The \code{findSubsets()} function finds all row numbers in the 
implicant matrix which correspond to all possible subsets of an implicant.
}

\value{
A vector of the line numbers of all possible subsets in the implicant matrix
}

\references{
A. Dusa. \emph{Enhancing Quine-McCluskey}. WP 2007-49, COMPASSS, 2007.\cr
URL: http://www.compasss.org/wpseries/Dusa2007b.pdf.

A. Dusa. A Mathematical Approach to the Boolean Minimization Problem.
\emph{Quality & Quantity}, 44(1):99-113, 2010.
}

\seealso{\code{\link{findSupersets}}}

\examples{
# three conditions with two levels each
noflevels <- c(2, 2, 2)

# the second row of the implicant matrix corresponding to the first level of 
# the rightmost condition
getRow(noflevels = noflevels + 1, row.no = 2) - 1

# the row numbers of the implicant matrix which correspond to subsets of this level  
(rows <- findSubsets(noflevels = noflevels + 1, 2))

# the corresponding matrix representation of these rows 
getRow(noflevels = noflevels + 1, row.no = rows) - 1

# stopping at row number 20
getRow(noflevels = noflevels + 1, row.no = findSubsets(noflevels + 1, 2, 20)) - 1
}

\keyword{functions}
