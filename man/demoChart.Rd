\name{demoChart}

\alias{demoChart}

\title{Create the Prime Implicants Chart}

\description{
This function creates a chart having the prime implicants on the rows and the observed combinations
of conditions on the columns. It is useful to determine visually which prime implicant (if any)
is essential. The chart is subsequently processed algorithmically to further reduce the reduntant
prime implicants. This function is for demonstration purposes only.
}

\usage{
demoChart(rows, columns, splitmethod = "")
}

\arguments{
  \item{rows}{A vector of strings, containing the prime implicants}                                 
  \item{columns}{A vector of strings, containing all combinations of conditions from the original data}
  \item{splitmethod}{String, to declare the separator of the input strings}
}

\value{
a logical matrix showing which conditions from the (minimized) prime implicants are found in 
which columns
}

\references{
W.V. Quine. The Problem of Simplifying Truth Functions. \emph{American Mathematical Monthly}, 
59(8):521-531, 1952.

C. C. Ragin. \emph{The Comparative Method: Moving beyond Qualitative and Quantitative Strategies}. 
University of California Press, Berkeley, 1987.

}

\seealso{\code{\link{prettyTable}}}

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

\keyword{functions}
