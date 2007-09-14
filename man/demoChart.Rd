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
demoChart(rows, columns, use.letters=TRUE)
}

\arguments{
  \item{rows}{a vector of strings, containing the prime implicants}                                 
  \item{columns}{a vector of strings, containing all combinations of conditions from the original data}
  \item{use.letters}{are letters used instead of column names}
}

\value{
a logical matrix showing which conditions from the (minimized) prime implicants are found in 
which columns
}

\references{
Quine, W.V. 1952 \emph{The Problem of Simplifying Truth Functions},
The American Mathematical Monthly, Vol. 59, No. 8. (Oct., 1952), pp. 521-531.

Ragin, Charles C. 1987 \emph{The Comparative Method. Moving beyond qualitative and
quantitative strategies}, Berkeley: University of California Press}

\author{
Adrian Dusa\cr
Romanian Social Data Archive\cr
adi@roda.ro\cr
Faculy of Sociology and Social Work, University of Bucharest\cr
adi@sas.unibuc.ro
}

\seealso{'prettyTable'}

\examples{

chart <- demoChart(c("A", "B", "c"), c("ABC", "Abc", "AbC", "aBc"))
prettyTable(chart)


# Quine's example, page 528
rows <- c("AB", "BC", "Ac", "aC", "abd", "bcd")
cols <- c("ABCD", "ABCd", "ABcD", "ABcd", "AbcD", "Abcd",
          "aBCD", "aBCd", "abCD", "abCd", "abcd")

chart <- demoChart(rows, cols)
prettyTable(chart)

}

\keyword{misc}
