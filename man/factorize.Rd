\name{factorize}

\alias{factorize}

\title{Factorize a Boolean Sum of Products}

\description{
This function finds all combinations of factorizing or factorized literals in a Boolean sum of products.
}

\usage{
factorize(bool.sum, prod.split = "", sort.factorizing = FALSE, sort.factorized = FALSE)
}

\arguments{
  \item{bool.sum}{An object containting either the list of solutions, or a single character solution}
  \item{prod.split}{The separator of the literals within a Boolean product}
  \item{sort.factorizing}{Logical, sort results beginning with largest number of factorizing literals}
  \item{sort.factorized}{Logical, sort results beginning with largest number of factorized literals}
}

\details{
In Boolean algebra, the multiplication (intersection) operator \eqn{*} is distributive over the addition 
(union) operator \eqn{+} such that for any three Boolean terms \eqn{a}, \eqn{b} and \eqn{c}, the following 
law holds: \eqn{(a*b) + (a*c) = a*(b + c)} (Hohn, 1966: 12; South, 1974: 12). The \code{factorize()} 
function finds all possible terms \eqn{a} for any Boolean sum of products. Solutions can be sorted in 
decreasing order by the number of factorizing literals or in decreasing order by the number of 
factorized literals. 
}

\references{
F. E. Hohn. \emph{Applied Boolean Algebra: An Elementary Introduction}. 2nd ed., Macmillan, New York, 1966.

G. F. South. \emph{Boolean Algebra and Its Uses}. Van Nostrand Reinhold, New York, 1974.
}

\seealso{\code{\link{eqmcc}}}

\examples{
# factorize any Boolean sum of products;
# if single letters are used, prod.split is not needed
factorize("AB + AC")

factorize("one*TWO*four + one*THREE + THREE*four", prod.split = "*")
factorize("~ONE*TWO*~FOUR + ~ONE*THREE + THREE*~FOUR", prod.split = "*")
factorize("one&TWO&four + one&THREE + THREE&four", prod.split = "&")

# factorize a QCA solution object directly
data(HarKem)
sol <- eqmcc(HarKem, outcome = "PB", conditions = c("C", "F", "T", "V"), 
  include = c("?", "C"))
factorize(sol)

data(Emme)
sol <- eqmcc(Emme, outcome = "JSR", incl.cut1 = 0.9)
factorize(sol)

# sort by largest number of factoring literals
factorize(sol, sort.factorizing = TRUE)

# sort by largest number of factorized literals
factorize(sol, sort.factorized = TRUE)

}

\keyword{functions}
