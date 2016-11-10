\name{factorize}

\alias{factorize}

\title{Factorize Boolean expressions}

\description{
This function finds all combinations of common factors in a Boolean expression
written in SOP - Sum Of Products form.
}

\usage{
factorize(expression, snames = "", noflevels, pos = FALSE, tilde, ...)
}

\arguments{
  \item{expression}{A string containing the SOP expression, or an object of
        class \code{"qca"}.}
  \item{snames}{A string containing the sets' names, separated by commas.}
  \item{noflevels}{Numerical vector containing the number of levels for each set.}
  \item{pos}{Logical, if possible factorize using product(s) of sums.}
  \item{tilde}{Logical, use a tilde to negate the expression.}
  \item{...}{Other arguments (mainly for backwards compatibility).}
}

\details{

Factorization is a process of finding common factors in a Boolean expression,
written in DNF - disjunctive normal form, or in a sum of products (SOP) form.
The factorization is also possible using products of sums (POS) form, whenever possible.

Starting with version 2.1 of the package, a number of arguments have been deprecated.
For example, \bold{\code{prod.split}}, because intersections (products) are
indicated by the standard \bold{\code{*}} sign, and sometimes products are not
indicated by anything (for example when the set names are single letters.

The argument \bold{\code{snames}} is only needed when products are not indicated by
any sign at all, and the set names have more than one letter each (see function
\bold{\code{translate()}} for more details).

The number of levels in \bold{\code{noflevels}} is needed only when negating multivalue
conditions, and it should complement the \bold{\code{snames}} argument.

If \bold{\code{expression}} is an object of class \code{"qca"} (the result of the
\bold{\code{eqmcc()}} minimization function), this function performs a factorization
for each of the minimized solutions.

}

\value{
A list with two components:

\tabular{rl}{
  \bold{initial} \tab {The input expression.}\cr
  \bold{factored} \tab {All possible factorizations of the input expression.}\cr
}

}

\author{
Adrian Dusa
}

\references{
Ragin, C.C. (1987) \emph{The Comparative Method. Moving beyond qualitative
and quantitative strategies}, Berkeley: University of California Press
}

\seealso{\code{\link{translate}}}

\examples{

# results presented in alphabetical order
factorize("one*TWO*four + one*THREE + THREE*four")

# to preserve a certain order of the set names
factorize("one*TWO*four + one*THREE + THREE*four",
          snames = "ONE, TWO, THREE, FOUR")

factorize("~ONE*TWO*~FOUR + ~ONE*THREE + THREE*~FOUR",
          snames = "ONE, TWO, THREE, FOUR")

# using pos - products of sums
factorize("a*c + a*D + b*c + b*D", pos = TRUE)


# using an object of class "qca" produced with eqmcc()
data(CVF)
pCVF <- eqmcc(CVF, outcome = "PROTEST", incl.cut = 0.8,
              include = "?", use.letters = TRUE)

factorize(pCVF)

\dontrun{
# using an object of class "deMorgan" produced with deMorgan()
factorize(deMorgan(pCVF))
}

}

\keyword{functions}
