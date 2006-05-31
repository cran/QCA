\name{qmcc}

\alias{qmcc}

\title{Find a minimum solution in a boolean procedure, using the Quine-McCluskey algorithm}

\description{
This function is the core of the QCA (Qualitative Comparative Analysis) package. The QCA is the
bridge between the qualitative and quantitative worlds, and its fundamental philosophy is based
on Mill's methods of inductive inquiry. Given a dataset with cases on the rows
(or better said, case studies) and various conditions on the columns, this function finds
which condition (or combination of conditions) is necessary and/or sufficient to trigger the outcome.
}

\usage{
qmcc(mydata, outcome = "", conditions = c(""), incl.rem = FALSE,
     expl.1 = FALSE, expl.0 = FALSE, expl.ctr = FALSE, expl.mo = FALSE,
     incl.1 = FALSE, incl.0 = FALSE, incl.ctr = FALSE, incl.mo = FALSE,
     quiet = FALSE, details = FALSE, chart = FALSE,
     use.letters = TRUE, show.cases = FALSE, tt = FALSE)
}

\arguments{
  \item{mydata}{the dataset used for minimization (dataframe or matrix)}
  \item{outcome}{the name of the outcome variable from the dataset}
  \item{conditions}{the name of the conditions from the dataset (if not specified, 
  all variables but the outcome are considered conditions)}
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
  \item{details}{prints some relevant details (set to FALSE if quiet is TRUE}
  \item{chart}{print the prime implicants chart}
  \item{use.letters}{should letters be used instead of column names}
  \item{show.cases}{show the lines corresponding to every minimized prime implicant}
  \item{tt}{specifies whether the input data is already a truth table}
}

\references{Ragin, Charles C. 1987 \emph{The Comparative Method. Moving beyond qualitative
and quantitative strategies}, Berkeley: University of California Press}

\author{
Adrian Dusa\cr
Romanian Social Data Archive, University of Bucharest\cr
adi@roda.ro
}

\note{The function finds a solution very quickly for a small number of conditions.
For eight conditions, it finds a solution in maximum 22 seconds, for nine conditions
in maximum 15 minutes. For 10 or more conditions, it needs A LOT of memory and it takes
a very long time to find a solution. Different other algorithms will be implemented
for these cases
}

\seealso{'truthTable'}

\examples{
data(Osa)

# find a solution using the Quine-McCluskey (qmcc) algorithm
# explaining only the presence of the outcome
qmcc(Osa, outcome="OUT", expl.1=TRUE)

# now including the remainers and the contradictions
qmcc(Osa, outcome="OUT", expl.1=TRUE, incl.rem=TRUE, incl.ctr=TRUE)

# the same as above, but we want to see the prime implicants chart
qmcc(Osa, outcome="OUT", expl.1=TRUE, incl.rem=TRUE, incl.ctr=TRUE, chart=TRUE)

# even more details
qmcc(Osa, outcome="OUT", expl.1=TRUE, incl.rem=TRUE, incl.ctr=TRUE, details=TRUE)

# printing the lines corresponding to each prime implicant
qmcc(Osa, outcome="OUT", expl.1=TRUE, incl.rem=TRUE, incl.ctr=TRUE, show.cases=TRUE)

# now explaining the absence of the outcome
qmcc(Osa, outcome="OUT", expl.0=TRUE, incl.rem=TRUE, incl.ctr=TRUE, show.cases=TRUE)


}

\keyword{misc}
