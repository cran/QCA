\name{qmcc}

\alias{qmcc}

\title{Find a minimum solution in a boolean procedure, using the Quine-McCluskey algorithm}

\description{
This function is the core of the QCA (Qualitative Comparative Analysis) package. The QCA is the
bridge between the qualitative and quantitative worlds, and its fundamental philosophy is based
on Mill's methods of inductive inquiry. Given a dataset with cases on the rows
(or better said, case studies) and various conditions on the columns, this function finds
which condition (or combination of conditions) is necessary and/or sufficient to trigger the outcome.
As of version 0.6-0, qmcc() is explicitely working with binary data only.
}

\usage{
qmcc(mydata, outcome = "", conditions = c(""), incl.rem = FALSE,
     expl.1 = FALSE, expl.0 = FALSE, expl.ctr = FALSE, expl.mo = FALSE,
     incl.1 = FALSE, incl.0 = FALSE, incl.ctr = FALSE, incl.mo = FALSE,
     quiet = FALSE, details = TRUE, chart = FALSE, use.letters = TRUE,
     show.cases = FALSE, diffmat=TRUE)
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
  \item{details}{prints some relevant details (set to FALSE if quiet is TRUE}
  \item{chart}{print the prime implicants chart}
  \item{use.letters}{should letters be used instead of column names}
  \item{show.cases}{show the lines corresponding to every minimized prime implicant}
  \item{diffmat}{generate the differences matrix}
}

\references{
Ragin, Charles C. (1987) \emph{The Comparative Method. Moving beyond qualitative
and quantitative strategies}, Berkeley: University of California Press

Dusa, Adrian (2007) \emph{A mathematical approach to the boolean minimization problem}, available on
http://www.compasss.org/files/WPfiles/Dusa2007.pdf
}

\author{
Adrian Dusa\cr
University of Bucharest, Faculty of Sociology and Social Work\cr
dusa.adrian@unibuc.ro
}

\note{The function finds an exact solution very quickly for relatively small number of conditions.
With a 2 GHz Intel Core2Duo processor and 1 GB of RAM, the algorithm finds a solution for 10
conditions in a less than 1 second. For 11 conditions it takes about 2.5 seconds, for 12
conditions it takes 9 seconds, for 13 conditions it takes about 37 seconds and for 14 conditions
it takes about 130 seconds (a little over 2 minutes).
For more than 14 conditions more memory is definitely needed; choosing not to create the
differences matrix (which requires a lot of RAM), the analysis can squeeze a result for 15
conditions at the expense of 10 minutes of computing time.
}

\seealso{\code{\link{truthTable}}, \code{\link{eqmcc}}}

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
