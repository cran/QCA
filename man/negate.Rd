\name{negate}

\alias{negate}
\alias{deMorgan}

\title{Negate Boolean expressions}

\description{
Function to negate a SOP expression.
}

\usage{
negate(input, snames = "", noflevels = NULL, use.tilde = FALSE)
}

\arguments{
  \item{input}{A string representing a SOP expression, or a minimization
        object of class \code{"qca"}.}
  \item{snames}{A string containing the sets' names, separated by commas.}
  \item{noflevels}{Numerical vector containing the number of levels for each set.}
  \item{use.tilde}{Logical, use a tilde to negate the sets.}
}

\details{

In Boolean algebra, there are two transformation rules named after the British
mathematician Augustus De Morgan. These rules state that:

1. The complement of the union of two sets is the intersection of their complements.

2. The complement of the intersection of two sets is the union of their complements.

In "normal" language, these would be written as:

1. \code{not (A and B) = (not A) or (not B)}

2. \code{not (A or B) = (not A) and (not B)}

Based on these two laws, any Boolean expression written in disjunctive normal form
can be transformed into its negation.

It is also possible to negate all models and solutions from the result of a Boolean
minimization from function \bold{\code{\link[QCA]{minimize}()}} in package \bold{\code{QCA}}.
The resulting object, of class \code{"qca"}, is automatically recognised by this function.

In a SOP expression, the products should normally be split by using a star \bold{\code{*}}
sign, otherwise the sets' names will be considered the individual letters in alphabetical
order, unless they are specified via \bold{\code{snames}}.

To negate multilevel expressions, the argument \bold{\code{noflevels}} is required.

It is entirely possible to obtain multiple negations of a single expression, since the
result of the negation is passed to function \bold{\code{\link[QCA]{simplify}()}}.
}

\value{
A character vector when the input is a SOP expresison, or a named list for minimization
input objects, each component containing all possible negations of the model(s).
}

\author{
Adrian Dusa
}

\references{
Ragin, Charles C. 1987. \emph{The Comparative Method: Moving beyond Qualitative 
and Quantitative Strategies}. Berkeley: University of California Press.
}

\seealso{\code{\link[QCA]{minimize}}}

\examples{

# example from Ragin (1987, p.99)
negate("AC + B~C")

# with different intersection operators
negate("AB*EF + ~CD*EF")


# using an object of class "qca" produced with minimize()
cLC <- minimize(LC, outcome = "SURV")

negate(cLC)


# parsimonious solution
pLC <- minimize(LC, outcome = "SURV", include = "?")

negate(pLC)
}

\keyword{functions}
