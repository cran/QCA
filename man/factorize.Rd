\name{factorize}

\alias{factorize}

\title{Factorize a QCA Solution}

\description{
This function finds all combinations of common factors of literals in a solution.
}

\usage{
factorize(sol.obj, splitmethod = "", sort.by.literals = FALSE, sort.by.number = FALSE)
}

\arguments{
  \item{sol.obj}{An object containting either the list of solutions, or a single character solution}
  \item{splitmethod}{The separator of the literals within a Boolean product}
  \item{sort.by.literals}{Logical, sort results by the largest number of literals as common factor}
  \item{sort.by.number}{Logical, sort results by the largest number of elements that have been factorized}
}

\references{
C. C. Ragin. \emph{The Comparative Method: Moving beyond Qualitative and Quantitative Strategies}. 
University of California Press, Berkeley, 1987.
}

\seealso{\code{\link{eqmcc}}}

\examples{
factorize("AB + AC + CD")

factorize("one*TWO*four + one*THREE + THREE*four", splitmethod = "*")

# factorize solution object directly
data(Emme)
sol <- eqmcc(Emme, outcome = "JSR", incl.cut1 = 0.9)
factorize(sol)

# sort by largest number of factoring literals
factorize(sol, sort.by.literals = TRUE)

# sort by largest number of factorized elements
factorize(sol, sort.by.number = TRUE)

}

\keyword{functions}
