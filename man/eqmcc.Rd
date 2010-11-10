\name{eqmcc}

\alias{eqmcc}

\title{Enhanced Quine-McCluskey algorithm}

\description{
This function is designed to improve the speed and memory problems of qmcc()
and it has very high chances to replace it in the future. All tests have been
successful, the solutions are exactly similar down to the last literal.
It has about the same options as qmcc(), minus the "details" and "diffmatrix"
arguments which are no longer required.
It is called "eqmcc" because it doesn't follow the classic minimization algorithm but
a more direct and rapid 'e'nhancement.
As of version 0.6-0, eqmcc() accepts multi-valued data.
}

\usage{
eqmcc(mydata, outcome = "", conditions = c(""), incl.rem = FALSE,
     expl.1 = FALSE, expl.0 = FALSE, expl.ctr = FALSE, expl.mo = FALSE,
     incl.1 = FALSE, incl.0 = FALSE, incl.ctr = FALSE, incl.mo = FALSE,
     quiet = FALSE, chart = FALSE, use.letters = TRUE, show.cases = FALSE,
     uplow=TRUE)
}

\arguments{
  \item{mydata}{a truth table (an R object with class "truthTable"), or the dataset used
  for minimization (either as a dataframe or as a matrix)}
  \item{outcome}{the name of the outcome variable from the dataset}
  \item{conditions}{a string vector containing the conditions' names from the dataset
  (if not specified, all variables but the outcome are included)}
  \item{incl.rem}{logical, include the remainders in the minimization procedure}
  \item{expl.1}{logical, explain the outcomes equal to 1}
  \item{expl.0}{logical, explain the outcomes equal to 0}
  \item{expl.ctr}{logical, explain the contradictions}
  \item{expl.mo}{logical, explain the missing outcomes (not implemented yet)}
  \item{incl.1}{logical, include the outcomes equal to 1 in the minimization procedure}
  \item{incl.0}{logical, include the outcomes equal to 0 in the minimization procedure}
  \item{incl.ctr}{logical, include the contradictions in the minimization procedure}
  \item{incl.mo}{logical, include the missing outcome in the minimization procedure (not implemented yet)}
  \item{quiet}{logical, print the solution without any other information}
  \item{chart}{logical, print the prime implicants chart}
  \item{use.letters}{logical, should letters be used instead of column names}
  \item{show.cases}{logical, show the lines corresponding to every minimized prime implicant}
  \item{uplow}{logical, use upper/lower notation if possible, for binary data}
}

\value{an invisible list with five components: initial combinations, minimised PIs,
the raw PI chart, the number of k PIs to solve the PI chart and the solution(s)}

\references{
Dusa, Adrian (2007a) \emph{Enhancing Quine-McCluskey}
http://www.compasss.org/files/WPfiles/Dusa2007a.pdf
}

\author{
Adrian Dusa\cr
University of Bucharest, Faculty of Sociology and Social Work\cr
dusadrian@unibuc.ro
}

\note{The speed is greatly improved: for bvQCA and 15 causal conditions it takes less than 2 minutes,
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

# for multi-value data
data(MV)
eqmcc(MV, outcome="OUT", expl.1=TRUE, incl.rem=TRUE, show.cases=TRUE)

}

\keyword{misc}
