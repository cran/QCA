\name{truth.table}

\alias{truth.table}

\title{Create a truth table for crisp sets}

\description{
For any number of conditions, there is a finite number of possible combinations of presence/absence.
This function finds the observed combinations among all possible ones, prints the frequency
of each observed combination and establishes the value for the outcome in this way:\cr
- if all observed combinations have an outcome value of 0 (or 1), then the value for the outcome
will be set to 0 (or 1)\cr
- if, for any given combination, the outcome presents both 0s and 1s, then the value for the
outcome will be a contradiction ("C")\cr
- for all the other possible combinations, the outcome is missing and will be coded with "?"
}

\usage{
truth.table(mydata, outcome = "", inside = FALSE, complete = FALSE,
            show.lines = FALSE)
}

\arguments{
  \item{mydata}{the dataset we use for minimization}
  \item{outcome}{the name of the outcome variable in the dataset}
  \item{inside}{if this function is called inside the "qmcc" function, it doesn't do formatting
  and deletes the column with the corresponding rownames for each combination of conditions}
  \item{complete}{prints the complete truth table, including the missing combinations}
  \item{show.lines}{show the rownames from the original dataset for each combination of conditions}
}

\references{Ragin, Charles C. 1987 \emph{The Comparative Method. Moving beyond qualitative 
and quantitative strategies}, Berkeley: University of California Press}

\author{
Adrian Dusa\cr
Romanian Social Data Archive, University of Bucharest\cr
adi@roda.ro
}

\seealso{'qmcc', 'print', 'cat'}

\examples{
data(Osa)

# print the truth table
truth.table(Osa, outcome="OUT", show.lines=TRUE)

# print the complete truth table
truth.table(Osa, outcome="OUT", show.lines=TRUE, complete=TRUE)
}

\keyword{misc}
