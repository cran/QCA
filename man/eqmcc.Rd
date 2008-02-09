\name{eqmcc}

\alias{eqmcc}

\title{Enhanced Quine-McCluskey algorithm}

\description{
This function is designed to improve the speed and memory problems that qmcc() currently hasm
and it has very high chances to replace the main qmcc() in the future; all tests have been
successful, the solutions are exactly the same down to the last literal.
It has about the same options as qmcc(), minus the "details" and "diffmatrix"
arguments which are no longer required.
It is called "eqmcc" because it doesn't follow the classic minimization algorithm but
a more direct and rapid 'e'nhancement.
}

\usage{
eqmcc(mydata, outcome = "", conditions = c(""), incl.rem = FALSE,
     expl.1 = FALSE, expl.0 = FALSE, expl.ctr = FALSE, expl.mo = FALSE,
     incl.1 = FALSE, incl.0 = FALSE, incl.ctr = FALSE, incl.mo = FALSE,
     quiet = FALSE, chart = FALSE, use.letters = TRUE, show.cases = FALSE)
}

\arguments{
  \item{mydata}{a truth table (an R object with class "truthTable"), or the dataset used
  for minimization (either as a dataframe or as a matrix)}
  \item{outcome}{the name of the outcome variable from the dataset}
  \item{conditions}{a string vector containing the conditions' names from the dataset
  (if not specified, all variables but the outcome are included)}
  \item{incl.rem}{include the remainders in the minimization procedure}
  \item{expl.1}{explain the outcomes equal to 1}
  \item{expl.0}{explain the outcomes equal to 0}
  \item{expl.ctr}{explain the contradictions}
  \item{expl.mo}{explain the missing outcomes (not implemented yet)}
  \item{incl.1}{include the outcomes equal to 1 in the minimization procedure}
  \item{incl.0}{include the outcomes equal to 0 in the minimization procedure}
  \item{incl.ctr}{include the contradictions in the minimization procedure}
  \item{incl.mo}{include the missing outcome in the minimization procedure (not implemented yet)}
  \item{quiet}{print the solution without any other information}
  \item{chart}{print the prime implicants chart}
  \item{use.letters}{should letters be used instead of column names}
  \item{show.cases}{show the lines corresponding to every minimized prime implicant}
}

\references{
Ragin, Charles C. 1987 \emph{The Comparative Method. Moving beyond qualitative
and quantitative strategies}, Berkeley: University of California Press

Dusa, Adrian 2007 \emph{Enhancing Quine-McCluskey}, http://www.compasss.org/WPShort.htm
}

\author{
Adrian Dusa\cr
Romanian Social Data Archive\cr
adi@roda.ro\cr
Faculty of Sociology and Social Work, University of Bucharest\cr
dusa.adrian@unibuc.ro
}

\note{The speed is greatly improved: for 15 causal conditions it takes less than 2 minutes,
compared to 10 minutes in qmcc(). The memory consumption is tiny by comparison: also for
15 causal conditions, qmcc() uses about 1.5 GB of RAM, where eqmcc() uses about 50 MB.
}

\seealso{\code{\link{truthTable}}, \code{\link{qmcc}}}

\examples{
data(Osa)

# explaining only the presence of the outcome
eqmcc(Osa, outcome="OUT", expl.1=TRUE)

# now including the remainers and the contradictions
eqmcc(Osa, outcome="OUT", expl.1=TRUE, incl.rem=TRUE, incl.ctr=TRUE)

# the same as above, but we want to see the prime implicants chart
eqmcc(Osa, outcome="OUT", expl.1=TRUE, incl.rem=TRUE, incl.ctr=TRUE, chart=TRUE)

# printing the lines corresponding to each prime implicant
eqmcc(Osa, outcome="OUT", expl.1=TRUE, incl.rem=TRUE, incl.ctr=TRUE, show.cases=TRUE)

# now explaining the absence of the outcome
eqmcc(Osa, outcome="OUT", expl.0=TRUE, incl.rem=TRUE, incl.ctr=TRUE, show.cases=TRUE)


}

\keyword{misc}
