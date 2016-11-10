\name{intersection}

\alias{intersection}

\title{Intersect two expressions}

\description{
This function takes two QCA expressions (combinations of conjunctions and disjunctions)
and finds their intersection.
}

\usage{
intersection(e1 = "", e2 = "", snames = "")
}

\arguments{
    \item{e1}{String, the first expression to intersect.}
    \item{e2}{String, the first expression to intersect.}
    \item{snames}{A string containing the set names, separated by commas.}
}

\details{
The aim of this function is to provide a software implementation of the intersection
examples presented by Ragin (1987: 144-147).

It can only deal with expressions using bivalent crisp-set conditions only, it doesn't
deal with multivalent crisp-set conditions because it requires additional information
about the number of levels in each causal condition.

The two expressions can be formulated in terms of either lower case - upper case notation
for the absence and the presence of the causal condition, or use the tilde notation
(see examples below). Usage of either of these is automatically detected, as long as both
expressions use the same notation.

If the \bold{\code{snames}} argument is provided, the result is sorted according to the order
of the causal conditions (set names) in the original dataset, otherwise it sorts the causal
conditions in alphabetical order.
}

\author{
Adrian Dusa
}


\references{
Ragin, Charles C. 1987. \emph{The Comparative Method: Moving beyond Qualitative and 
Quantitative Strategies}. Berkeley: University of California Press.
}


\examples{

# in Ragin's (1987) book, the equation E = SG + LW is the result
# of the Boolean minimization for the ethnic political mobilization.

# intersecting the reactive ethnicity perspective (R = lw)
# with the equation E (page 144)
intersection("lw", "SG + LW", snames = "S, L, W, G")

# [1] "SlwG"



# resources for size and wealth (C = SW) with E (page 145)
intersection("SW", "SG + LW", snames = "S, L, W, G")

# [1] "SWG + SLW"


# and factorized
factorize(intersection("SW", "SG + LW", snames = "S, L, W, G"))

# F1: SW(G + L)


# developmental perspective (D = Lg) and E (page 146)
intersection("Lg", "SG + LW", snames = "S, L, W, G")

# [1] "LWg"


# subnations that exhibit ethic political mobilization (E) but were
# not hypothesized by any of the three theories (page 147)
# ~H = ~(lw + SW + Lg)

intersection(deMorgan("lw + SW + Lg"), "SG + LW", snames = "S, L, W, G")

# [1] "sLWG + SLwG"

}

\keyword{functions}
