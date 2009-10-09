\name{solveChart}

\alias{solveChart}

\title{Reduce the redundant prime implicants}

\description{
While the minimization procedure finds a certain number of prime implicants, not all of them
are necessary as some are already covered by others (therefore redundant). This function further
reduces the number of prime implicants for a minimum solution.
}

\usage{
solveChart(chart)
}

\arguments{
  \item{chart}{the prime implicants chart, a matrix with TRUE/FALSE values}
}

\value{
a matrix containing row indices of all possible solutions
}

\references{Ragin, Charles C. (1987) \emph{The Comparative Method. Moving beyond qualitative 
and quantitative strategies}, Berkeley: University of California Press}

\author{
Adrian Dusa\cr
University of Bucharest, Faculty of Sociology and Social Work\cr
dusa.adrian@unibuc.ro
}

\seealso{\code{\link{createChart}}}

\examples{
# the chart we're trying to further reduce
chart <- demoChart(c("A", "B", "c"), c("ABC", "Abc", "AbC", "aBc"))
prettyTable(chart)

solveChart(chart)

## two minimum solutions: first and second or first and third prime implicants
}

\keyword{misc}
