\name{solveChart}

\alias{solveChart}

\title{Reduce the Redundant Prime Implicants}

\description{
While the minimization procedure finds a certain number of prime implicants, not all of them
are necessary. This function reduces the number of prime implicants to a minimal solution.
}

\usage{
solveChart(chart)
}

\arguments{
  \item{chart}{The prime implicants chart, a matrix with TRUE/FALSE values}
}

\value{
a matrix containing row indices of all possible solutions
}

\references{
C. C. Ragin. \emph{The Comparative Method: Moving beyond Qualitative and Quantitative Strategies}. 
University of California Press, Berkeley, 1987.
}


\seealso{\code{\link{createChart}}}

\examples{
# the chart to be reduced
chart <- demoChart(c("A", "B", "c"), c("ABC", "Abc", "AbC", "aBc"))
prettyTable(chart)

# solution: first and second or first and third prime implicants
solveChart(chart)

}

\keyword{functions}
