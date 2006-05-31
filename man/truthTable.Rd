\name{truthTable}

\alias{truthTable}

\title{Create a truth table for crisp sets}

\description{
For any number of conditions, there is a finite number of possible combinations of presence/absence.
This function finds the observed combinations among all possible ones, prints the frequency
of each observed combination and establishes the value for the outcome in this way:\cr
- if all observed combinations agree on having the same outcome value (either 0 or 1), then the 
value for the outcome will be set to that value\cr
- for any given combination, if the outcome present values of both 0 and 1 then the value for the
outcome will be set to a contradiction ("C")\cr
- for all other possible combinations, the outcome is missing and will be coded with "?"
}

\usage{
truthTable(mydata, outcome = "", conditions = c(""), inside = FALSE,
           complete = FALSE, show.cases = FALSE)
}

\arguments{
  \item{mydata}{the dataset we use for minimization}
  \item{outcome}{the name of the outcome variable in the dataset}
  \item{conditions}{the name of the conditions from the dataset (if not specified, all variables
  but the outcome are considered conditions)}
  \item{inside}{if this function is called inside the "qmcc" function, it doesn't do formatting
  and deletes the column with the corresponding rownames for each combination of conditions}
  \item{show.cases}{show the rownames from the original dataset for each combination of conditions}
  \item{complete}{prints the complete truth table, including the missing combinations}
}

\references{Ragin, Charles C. 1987 \emph{The Comparative Method. Moving beyond qualitative 
and quantitative strategies}, Berkeley: University of California Press}

\author{
Adrian Dusa\cr
Romanian Social Data Archive, University of Bucharest\cr
adi@roda.ro
}

\seealso{'qmcc'}

\examples{
data(Osa)

# print the truth table
truthTable(Osa, outcome="OUT", show.cases=TRUE)

# print the complete truth table
truthTable(Osa, outcome="OUT", show.cases=TRUE, complete=TRUE)
}

\keyword{misc}
