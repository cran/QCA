\name{PI chart functions: makeChart, solveChart}

\alias{makeChart}
\alias{solveChart}

\title{Create and solve a prime implicants chart}

\description{
These functions help creating a demo for a prime implicant chart, and also
show how to solve it using a minimum number of prime implicants.  
}

\usage{
makeChart(primes = "", configs = "", snames = "")

solveChart(chart, row.dom = FALSE, all.sol = FALSE, ...)
}

\arguments{
  \item{primes}{A string containing prime implicants, separated by commas, or a
        matrix of implicants.}                                
  \item{configs}{A string containing  causal configurations, separated by commas,
        or a matrix of causal configurations in the implicants space.}
  \item{snames}{A string containing the sets' names, separated by commas.}
  \item{chart}{A logical matrix (as the PI chart).}
  \item{row.dom}{Logical, apply row dominance to eliminate redundant prime implicants.}
  \item{all.sol}{Derive all possible solutions, irrespective of the number of PIs.}
  \item{...}{Other arguments (mainly for backwards compatibility).}
}

\details{
A PI chart, in this package, is a logical matrix (with TRUE/FALSE values), containing
the prime implicants on the rows and the starting configurations of causal conditions,
on the columns, like the one produced by \bold{\code{makeChart()}}. It is useful to
determine visually which prime implicant (if any) is essential.

When \bold{\code{primes}} and \bold{\code{configs}} are character, the individual sets
are identified using the function \bold{\code{translate()}}, using the SOP - Sum Of
Products form, which needs the set names in the absence of any other information. If
products are formed using the standard \bold{\code{*}} operator, specifying the set names
is not mandatory.

When \bold{\code{primes}} and \bold{\code{configs}} are matrices, they have to be specified
at implicants level, where the value \bold{\code{0}} is interpreted as a minimized literal.

The chart is subsequently processed algorithmically by \bold{\code{solveChart()}} to further
reduce the reduntant prime implicants. It applies a linear programming function from
package \pkg{lpSolve}, to find the absolute minimal number \code{M} of rows (prime
implicants) necessary to cover all columns, then searches through all possible combinations
of \code{M} rows, to find those which actually cover the columns.

Since all possible combinations grow exponentially with the number of prime implicants
resulted from the Quine-McCluskey minimization procedure, this function tries hard to
reduce this number by first eliminating the redundant prime implicants.

When set to \bold{\code{TRUE}}, the argument \bold{\code{row.dom}} does something like this by
eliminating the dominated rows (those which cover a smaller number of columns than
another, dominant prime implicant).
}

\value{
For \bold{\code{makeChart}}: a logical matrix.

For \bold{\code{solveChart}}: a matrix containing all possible combinations of PI chart rows
necessary to cover all its columns.
}

\author{
Adrian Dusa
}

\references{
Quine, W.V. (1952) \emph{The Problem of Simplifying Truth Functions},
The American Mathematical Monthly, vol.59, no.8. (Oct., 1952), pp.521-531.

Ragin, Charles C. (1987) \emph{The Comparative Method. Moving beyond qualitative and
quantitative strategies}, Berkeley: University of California Press
}

\examples{

# non-standard products, it needs the set names
chart <- makeChart("A, B, c", "ABC, Abc, AbC, aBc", snames =  "A,B,C")
prettyTable(chart)
#   ABC Abc AbC aBc
# A  x   x   x   - 
# B  x   -   -   x 
# c  -   x   -   x 

solveChart(chart)
# first and second rows (A + B)
# and first and third rows (A + c)
# A is an essential prime implicant
#      A + B  A + c
#      [,1]   [,2]
# [1,]    1      1
# [2,]    2      3


# Quine's example, page 528
rows <- c("AB, BC, Ac, aC, abd, bcd")
cols <- c("ABCD, ABCd, ABcD, ABcd, AbcD, Abcd, aBCD, aBCd, abCD, abCd, abcd")

prettyTable(chart <- makeChart(rows, cols, "A,B,C,D"))
#     ABCD ABCd ABcD ABcd AbcD Abcd aBCD aBCd abCD abCd abcd
# AB   x    x    x    x    -    -    -    -    -    -    -  
# BC   x    x    -    -    -    -    x    x    -    -    -  
# Ac   -    -    x    x    x    x    -    -    -    -    -  
# aC   -    -    -    -    -    -    x    x    x    x    -  
# abd  -    -    -    -    -    -    -    -    -    x    x  
# bcd  -    -    -    -    -    x    -    -    -    -    x 

solveChart(chart)
#      [,1] [,2] [,3] [,4]
# [1,]    1    1    2    2
# [2,]    3    3    3    3
# [3,]    4    4    4    4
# [4,]    5    6    5    6


# using SOP standard product sign
rows <- c("EF, ~GH, IJ")
cols <- c("~EF*~GH*IJ, EF*GH*~IJ, ~EF*GH*IJ, EF*~GH*~IJ")
prettyTable(chart <- makeChart(rows, cols))
#     ~EF*~GH*IJ EF*GH*~IJ ~EF*GH*IJ EF*~GH*~IJ
# EF      -          x         -         x     
# ~GH     x          -         -         x     
# IJ      x          -         x         -     

solveChart(chart)
# ~GH is redundant
#     EF + IJ
#      [,1]
# [1,]    1
# [2,]    3


# using implicant matrices
primes <- matrix(c(2,2,1,0,2,2,0,2,2,2), nrow = 2)
configs <- matrix(c(2,2,2,1,1,2,2,2,2,1,2,2,2,2,2), nrow = 3)
colnames(primes) <- colnames(configs) <- LETTERS[1:5]

# the prime implicants: AbCE and ACDE
primes
#      A B C D E
# [1,] 2 1 2 0 2
# [2,] 2 0 2 2 2

# the initial causal combinations: AbCdE, AbCDE and ABCDE
configs
#      A B C D E
# [1,] 2 1 2 1 2
# [2,] 2 1 2 2 2
# [3,] 2 2 2 2 2

chartLC <- makeChart(primes, configs)
prettyTable(chartLC)
#      AbCdE AbCDE ABCDE
# AbCE   x     x     -  
# ACDE   -     x     x

}

\keyword{functions}
