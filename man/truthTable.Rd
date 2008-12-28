\name{truthTable}
\alias{truthTable}
\alias{print.tt}
\alias{is.tt}

\title{Truth table}

\description{
For any number of conditions, there is a finite number of possible combinations of
presence/absence.\cr
\code{truthTable()} finds the observed combinations among all possible ones, prints the frequency
of each observed combination and establishes the value for the outcome in this way:\cr
- if all observed combinations agree on having the same outcome value (either 0 or 1), then the 
value for the outcome will be set to that value\cr
- for any given combination, if the outcome present values of both 0 and 1 then the value for the
outcome will be set to a contradiction ("C")\cr
- for all other possible combinations, the outcome is missing and will be coded with "?"\cr
\cr
\code{is.tt()} checkes if an object has the class tt (if it is a truth table); such an
object is created by truthTable()\cr
\cr
\code{print.tt()} has an S3 method for printing objects of class 'tt'
}

\usage{
truthTable(mydata, outcome = "", conditions = c(""), complete = FALSE,
           show.cases = FALSE, quiet = FALSE)

is.tt(x)

\method{print}{tt}(x, funqmcc=FALSE, ...)
}

\arguments{
  \item{mydata}{the dataset we use for minimization}
  \item{outcome}{the name of the outcome variable in the dataset}
  \item{conditions}{the name of the conditions from the dataset (if not specified, all variables
  but the outcome are considered conditions)}
  \item{quiet}{if TRUE, return the truth table invisibly}
  \item{show.cases}{show the rownames from the original dataset for each combination of conditions}
  \item{complete}{prints the complete truth table, including the missing combinations}
  \item{x}{an object of class tt}
  \item{funqmcc}{logical, if called by (e)qmcc() function(s) it prints only the observed
  combinations from the initial data}
  \item{...}{other arguments from the generic print (not used in this function)}
}

\value{
An object of class "tt", which is essentially a list with three components:\cr
    \tabular{ll}{
    \$tt: \tab the truth table itself\cr
    \$indexes: \tab a vector with the base 10 representation of the\cr
               \tab truth table observed combinations\cr
    \$noflevels: \tab a vector with the number of levels from all input\cr
                 \tab variables\cr
    }
}

\references{Ragin, Charles C. 1987 \emph{The Comparative Method. Moving beyond qualitative 
and quantitative strategies}, Berkeley: University of California Press}

\author{
Adrian Dusa\cr
Romanian Social Data Archive\cr
adi@roda.ro\cr
Faculty of Sociology and Social Work, University of Bucharest\cr
adi@sas.unibuc.ro
}

\seealso{\code{\link{qmcc}}, \code{\link{eqmcc}}}

\examples{
data(Osa)

# print the truth table
truthTable(Osa, outcome="OUT", show.cases=TRUE)

# print the complete truth table
truthTable(Osa, outcome="OUT", complete=TRUE, show.cases=TRUE)

# save the result into an R object:
mytable <- truthTable(Osa, outcome="OUT", complete=TRUE, show.cases=TRUE, quiet=TRUE)

mytable # or print.tt(mytable)

# check the components

mytable$tt # the truth table itself

mytable$indexes # base 10 representation of input combinations

mytable$noflevels # number of levels from each causal condition
}

\keyword{misc}
