\name{writeSolution}

\alias{writeSolution}

\title{Write a QCA solution}

\description{
This function takes as arguments two matrices containing the solution and the PI chart
and writes the corresponding QCA solution.
}

\usage{
writeSolution(sol, pic)
}

\arguments{
  \item{sol}{The matrix containing the solution space.}
  \item{pic}{The PI chart matrix.}
}

\details{
The solution matrix is essentially a 1 column matrix containing the result of the
\bold{\code{solveChart()}} function (the rownames of its resulting matrix).

The PI chart matrix is the result of the \bold{\code{makeChart()}} function.
}

\value{
A list containing the QCA solutions and the essential prime implicants.
}

\author{
Adrian Dusa
}

\examples{

expressions <- matrix(c(2, 2, 0, 1, 2, 2, 2, 0, 2, 2), nrow = 2)
colnames(expressions) <- c("DEV", "URB", "LIT", "IND", "STB")
rownames(expressions) <- c("DEV*LIT*IND*STB", "DEV*urb*LIT*STB")

#                 DEV URB LIT IND STB
# DEV*LIT*IND*STB   2   0   2   2   2
# DEV*urb*LIT*STB   2   1   2   0   2

inputt <- matrix(c(2, 2, 2, 1, 1, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2), nrow = 3)
colnames(inputt) <- c("DEV", "URB", "LIT", "IND", "STB")

pic <- makeChart(expressions, inputt)
colnames(pic) <- c(22, 24, 32)

#                    22   24    32
# DEV*LIT*IND*STB FALSE TRUE  TRUE
# DEV*urb*LIT*STB  TRUE TRUE FALSE

sol <- solveChart(pic)
sol <- matrix(rownames(pic)[sol], nrow=nrow(sol))

#      [,1]             
# [1,] "DEV*LIT*IND*STB"
# [2,] "DEV*urb*LIT*STB"


writeSolution(sol, pic)

}


\keyword{functions}
