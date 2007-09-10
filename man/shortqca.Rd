\name{shortqca}

\alias{shortqca}

\title{Find a minimum, exact solution equal to the Quine-McCluskey algorithm}

\description{
This function is designed to improve the memory problems that qmcc() currently has.
The version is experimental but it has very high chances to replace the main qmcc()
function; all tests have been successful, the solutions are exactly the same down to the
last literal.
This function has almost the same options as qmcc(), minus the "details" and "diffmatrix"
arguments which are no longer required.
It is called "shortqca" because it doesn't follow the classic minimization algorithm but
a more direct and rapid shortcut.
}

\usage{
shortqca(mydata, outcome = "", conditions = c(""), incl.rem = FALSE,
     expl.1 = FALSE, expl.0 = FALSE, expl.ctr = FALSE, expl.mo = FALSE,
     incl.1 = FALSE, incl.0 = FALSE, incl.ctr = FALSE, incl.mo = FALSE,
     quiet = FALSE, chart = FALSE, use.letters = TRUE, show.cases = FALSE)
}

\arguments{
  \item{mydata}{the dataset used for minimization (dataframe or matrix)}
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
  \item{quiet}{prints the solution without any other information}
  \item{chart}{print the prime implicants chart}
  \item{use.letters}{should letters be used instead of column names}
  \item{show.cases}{show the lines corresponding to every minimized prime implicant}
}

\references{Ragin, Charles C. 1987 \emph{The Comparative Method. Moving beyond qualitative
and quantitative strategies}, Berkeley: University of California Press}

\author{
Adrian Dusa\cr
Romanian Social Data Archive, University of Bucharest\cr
adi@roda.ro
}

\note{The speed is greatly improved: for 15 causal conditions it takes less than 2 minutes,
compared to 10 minutes in qmcc(). The memory consumption is tiny by comparison: also for
15 causal conditions, qmcc() uses about 1.5 GB of RAM, where shortqca() uses about 50 MB.
}

\seealso{'truthTable', 'qmcc'}

\examples{
data(Osa)

# explaining only the presence of the outcome
shortqca(Osa, outcome="OUT", expl.1=TRUE)

# now including the remainers and the contradictions
shortqca(Osa, outcome="OUT", expl.1=TRUE, incl.rem=TRUE, incl.ctr=TRUE)

# the same as above, but we want to see the prime implicants chart
shortqca(Osa, outcome="OUT", expl.1=TRUE, incl.rem=TRUE, incl.ctr=TRUE, chart=TRUE)

# printing the lines corresponding to each prime implicant
shortqca(Osa, outcome="OUT", expl.1=TRUE, incl.rem=TRUE, incl.ctr=TRUE, show.cases=TRUE)

# now explaining the absence of the outcome
shortqca(Osa, outcome="OUT", expl.0=TRUE, incl.rem=TRUE, incl.ctr=TRUE, show.cases=TRUE)


}

\keyword{misc}
