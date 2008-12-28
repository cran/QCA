\name{factorize}

\alias{factorize}

\title{Factorize a QCA solution}

\description{
This function finds all combinations of common factors of literals in a QCA solution
}

\usage{
factorize(my.string, use.letters=TRUE,
          sort.by.literals=FALSE, sort.by.number=FALSE)
}

\arguments{
  \item{my.string}{a string which contains a sequence of literals}
  \item{use.letters}{specify whether conditions' names are simple letters}
  \item{sort.by.literals}{sort results by the largest number of literals as common factor}
  \item{sort.by.number}{sort results by the largest number of elements that have been factorized}
}

\references{Ragin, Charles C. 1987 \emph{The Comparative Method. Moving beyond qualitative
and quantitative strategies}, Berkeley: University of California Press}

\author{
Adrian Dusa\cr
Romanian Social Data Archive\cr
adi@roda.ro\cr
Faculty of Sociology and Social Work, University of Bucharest\cr
adi@sas.unibuc.ro
}

\seealso{\code{\link{qmcc}}, \code{\link{eqmcc}}}

\examples{
factorize("AB + AC + CD")

factorize("one*TWO*four + one*THREE + THREE*four", use.letters=FALSE)
}

\keyword{misc}
