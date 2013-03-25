\name{demoChart}

\alias{demoChart}

\title{Create a Prime Implicant Chart}

\description{
This function creates prime implicant charts. It is for demonstration purposes only. 
}

\usage{
demoChart(rows, columns, splitmethod = "")
}

\arguments{
  \item{rows}{A vector of strings containing the prime implicants.}                                 
  \item{columns}{A vector of strings containing the fundamental products.}
  \item{splitmethod}{A string declaring the separator of the input strings.}
}

\details{
This function creates prime implicant (PI) charts as first proposed by Quine (1952) and 
McCluskey (1956). PI charts have the prime implicants listed on the rows and the fundamental 
products (the configurations from the truth table to be included in the minimization process) 
listed on the columns. This function is for demonstration purposes only. 
}

\value{
A logical matrix showing which columns are subsets of which rows.
}

\references{
E. J. McCluskey. Minimization of Boolean Functions. \emph{Bell Systems Technical Journal}, 
35(6):1417-1444, 1956.

W. V. Quine. The Problem of Simplifying Truth Functions. \emph{American Mathematical Monthly}, 
59(8):521-531, 1952.
}

\examples{
# a simple PI chart, formatted using internal function prettyTable()
PIs <- c("A", "B", "c") # the prime implicants
FPs <- c("ABC", "Abc", "AbC", "aBc") # the fundamental products

chart <- demoChart(PIs, FPs)
prettyTable(chart)

# example from Quine (1952, p.528)
PIs <- c("AB", "BC", "Ac", "aC", "abd", "bcd")
FPs <- c("ABCD", "ABCd", "ABcD", "ABcd", "AbcD", "Abcd",
         "aBCD", "aBCd", "abCD", "abCd", "abcd")

chart <- demoChart(PIs, FPs)
prettyTable(chart)
}

\keyword{functions}
