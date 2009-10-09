\name{factorize}

\alias{factorize}

\title{Factorize a QCA solution}

\description{
This function finds all combinations of common factors of literals in a QCA solution
}

\usage{
factorize(my.string, splitmethod="", sort.by.literals=FALSE, sort.by.number=FALSE)
}

\arguments{
  \item{my.string}{a string which contains a sequence of literals}
  \item{splitmethod}{string, to declare the separator of the input string}
  \item{sort.by.literals}{sort results by the largest number of literals as common factor}
  \item{sort.by.number}{sort results by the largest number of elements that have been factorized}
}

\references{Ragin, Charles C. (1987) \emph{The Comparative Method. Moving beyond qualitative
and quantitative strategies}, Berkeley: University of California Press}

\author{
Adrian Dusa\cr
University of Bucharest, Faculty of Sociology and Social Work\cr
dusa.adrian@unibuc.ro
}

\seealso{\code{\link{qmcc}}, \code{\link{eqmcc}}}

\examples{
factorize("AB + AC + CD")

factorize("one*TWO*four + one*THREE + THREE*four", splitmethod="*")
}

\keyword{misc}
