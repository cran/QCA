\name{solveChart}

\alias{solveChart}

\title{Eliminate Redundant Prime Implicants}

\description{
This function reduces the complete sum of prime implicants to a minimal sum by 
eliminating redundant prime implicants.
}

\usage{
solveChart(chart, all.sol = FALSE)
}

\arguments{
  \item{chart}{A prime implicant chart.}
  \item{all.sol}{Find all possible, non-overlapping, sums.}
}

\value{
A matrix containing row indices of all possible solutions.
}

\examples{
# a simple PI chart, formatted using internal function prettyTable()
PIs <- c("A", "B", "c", "D") # all prime implicants; the complete sum
FPs <- c("AbCD", "AbCd", "aBcd", "aBCD") # the fundamental products

chart <- demoChart(PIs, FPs)
prettyTable(chart)

# minimal sum: first or second PI: A + B
solveChart(chart)

# all sums: first or second PI (A + B);
# first or third or fourth PI (A + c + D)
solveChart(chart, all.sol = TRUE)
}

\keyword{functions}
