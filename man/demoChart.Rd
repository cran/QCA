\name{demoChart}

\alias{demoChart}

\title{Create the prime implicants chart}

\description{
This function creates a chart having the prime implicants on the rows and the observed combinations
of conditions on the columns. It is useful to determine visually which prime implicant (if any)
is essential. The chart is subsequently processed algorithmically to further reduce the reduntant
prime implicants.
This function is for demonstration purposes only. The internal function createChart() is faster
but its arguments are more complex.
}

\usage{
demoChart(rows, columns)
}

\arguments{
  \item{rows}{a vector of strings, containing the prime implicants}
  \item{columns}{a vector of strings, containing all combinations of conditions from the original data}
}

\value{
a logical matrix showing which conditions from the (minimized) prime implicants are found in 
which columns
}

\references{Ragin, Charles C. 1987 \emph{The Comparative Method. Moving beyond qualitative and
quantitative strategies}, Berkeley: University of California Press}

\author{
Adrian Dusa\cr
Romanian Social Data Archive, University of Bucharest\cr
adi@roda.ro
}

\seealso{'prettyTable'}

\examples{
chart <- demoChart(c("A", "B", "c"), c("ABC", "Abc", "AbC", "aBc"))

prettyTable(chart)
}

\keyword{misc}
