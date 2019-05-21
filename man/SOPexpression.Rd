\name{Interpret SOP/DNF expressions: compute, simplify}

\alias{compute}
\alias{simplify}
\alias{sop}

\title{Functions to interpret a SOP/DNF expression}

\description{
These functions interpret an expression written in a SOP - sum of products (or in canonical
DNF - disjunctive normal form), for both crisp and multivalue QCA. The function
\bold{\code{compute()}} calculates set membership scores based on a SOP expression applied
to a calibrated data set.

For crisp sets notation, upper case letters are considered the presence of that causal
condition, and lower case letters are considered the absence of the respective causal
condition. Tilde is recognized as a negation, even in combination with upper/lower letters.

A function similar to \bold{\code{compute()}} was initially written by Lewandowski (2015) but
the actual code in these functions has been completely re-written and expanded with more
extensive functionality (see details and examples below).

The function \bold{\code{simplify()}} transforms any expression (most notably a POS - product
of sums) into a simpler sum of products, minimizing it to the simplest equivalent logical
expression. It provides a software implementation of the intersection examples presented by
Ragin (1987: 144-147), and extended to multi-value sets.
}

\usage{
compute(expression = "", data, separate = FALSE)

simplify(expression = "", snames = "", noflevels, use.tilde = FALSE, ...)
}

\arguments{
  \item{expression}{String: a QCA expression written in sum of products form.}
  \item{data}{A dataset with binary cs, mv and fs data.}
  \item{separate}{Logical, perform computations on individual, separate paths.}
  \item{snames}{A string containing the sets' names, separated by commas.}
  \item{noflevels}{Numerical vector containing the number of levels for each set.}
  \item{use.tilde}{Logical, use tilde to negate bivalent conditions.}
  \item{...}{Other arguments, mainly for backwards compatibility.}
}

\details{
An expression written in SOP - sum of products, is a "union of intersections", for example
\bold{\code{A*B + B*c}}. The DNF - disjunctive normal form is also a sum of products, with
the restriction that each product has to contain all literals. The equivalent expression is:
\bold{\code{A*B*c + A*B*c + a*B*c}}.

The same expression can be written in multivalue notation: \bold{\code{A{1}*B{1} + B{1}*C{0}}}.
Both types of expressions are valid, and yield the same result on the same dataset.

For multivalue notation, causal conditions are expected as upper case letters, and they will be
converted to upper case by default. Expressions can contain multiple values to translate, separated
by a comma. If B was a multivalue causal condition, an expression could be:
\bold{\code{A{1} + B{1,2}*C{0}}}.

In this example, all values in B equal to either 1 or 2 will be converted to 1, and the
rest of the (multi)values will be converted to 0.

These functions automatically detects the use of tilde \dQuote{\code{~}} as a negation for a particular
causal condition. \bold{\code{~A}} does two things: it identifies the presence of causal
condition \bold{\code{A}} (because it was specified as upper case) and it recognizes that it
must be negated, because of the  tilde. It works even combined with lower case names:
\bold{\code{~a}}, which is interpreted as \bold{\code{A}}.

To negate a multivalue condition using a tilde, the number of levels should be supplied
(see examples below). Improvements in version 2.5 allow for intersections between multiple levels
of the same condition. For a causal condition with 3 levels (0, 1 and 2) the following expression
\bold{\code{~A{0,2}*A{1,2}}} is equivalent with \bold{\code{A{1}}}, while \bold{\code{A{0}*A{1}}}
results in the empty set.

The number of levels, as well as the set names can be automatically detected from a dataset via
the argument \bold{\code{data}}. Arguments \bold{\code{snames}} and \bold{\code{noflevels}} have
precedence over \bold{\code{data}}, when specified.

The use of the product operator \bold{\code{*}} is redundant when the set names are single
letters (for example \bold{\code{AD + Bc}}), and is also redundant for multivalue data, where 
product terms can be separated by using the curly brackets notation.

When conditions are binary and their names have multiple letters (for example
\bold{\code{AA + CC*bb}}), the use of the product operator \bold{\code{*}} is preferable but the
function manages to translate an expression even without it (\bold{\code{AA + CCbb}}) by searching
deep in the space of the conditions' names, at the cost of slowing down for a high number of causal
conditions. For this reason, an arbitrary limit of 7 causal \bold{\code{snames}} is imposed, to
write an expression.

For the function \bold{\code{simplify()}}, if a tilde is present in the expression, the argument
\bold{\code{use.tilde}} is automatically activated. For Boolean expressions, the simplest equivalent
logical expression can result in the empty set, if the conditions cancel each other out.
}


\value{
For the function \bold{\code{compute()}}, a vector of set membership values.

For function \bold{\code{simplify()}}, a character expression.
}

\author{
Adrian Dusa
}

\references{
Ragin, C.C. (1987) \emph{The Comparative Method: Moving beyond Qualitative and 
Quantitative Strategies}. Berkeley: University of California Press.

Lewandowski, J. (2015) QCAtools: Helper functions for QCA in R. R package version 0.1
}

\examples{
# -----
# for compute()
compute("DEV*ind + URB*STB", data = LF)

# calculating individual paths
compute("DEV*ind + URB*STB", data = LF, separate = TRUE)



# -----
# for simplify()
simplify("(A + B)(A + ~B)") # result is "A"

# to force a certain order of the set names
simplify("(URB + LIT*~DEV)(~LIT + ~DEV)", snames = "DEV, URB, LIT")

# multilevel conditions can also be specified (and negated)
simplify("(A{1} + ~B{0})(B{1} + C{0})", snames = "A, B, C", noflevels = c(2, 3, 2))


# in Ragin's (1987) book, the equation E = SG + LW is the result
# of the Boolean minimization for the ethnic political mobilization.

# intersecting the reactive ethnicity perspective (R = lw)
# with the equation E (page 144)

simplify("lw(SG + LW)", snames = "S, L, W, G")

# SlwG


# resources for size and wealth (C = SW) with E (page 145)
simplify("SW(SG + LW)", snames = "S, L, W, G")

# SLW + SWG


# and factorized
factorize(simplify("SW(SG + LW)", snames = "S, L, W, G"))

# SW(G + L)


# developmental perspective (D = Lg) and E (page 146)
simplify("Lg(SG + LW)", snames = "S, L, W, G", use.tilde = TRUE)

# LW~G

# subnations that exhibit ethnic political mobilization (E) but were
# not hypothesized by any of the three theories (page 147)
# ~H = ~(lw + SW + Lg) = GLs + GLw + GsW + lsW

simplify("(GLs + GLw + GsW + lsW)(SG + LW)", snames = "S, L, W, G")

# sLWG + SLwG
}

\keyword{functions}
