\name{createString}

\alias{createString}

\title{Create a vector of strings containing combinations of presence/absence of conditions}

\description{
The function takes the values from the dataset (0 or 1) and writes the corresponding lower/upper
column names.
}

\usage{
createString(mydata, use.letters=TRUE, uplow=TRUE)
}

\arguments{
  \item{mydata}{a matrix or a dataframe}
  \item{use.letters}{logical, use letters instead of column names}
  \item{uplow}{logical, use upper/lower notation if possible}
}

\value{
A vector of strings, with length equal to the number of rows in \bold{mydata}
}

\author{
Adrian Dusa\cr
Romanian Social Data Archive\cr
adi@roda.ro\cr
Faculty of Sociology and Social Work, University of Bucharest\cr
dusa.adrian@unibuc.ro
}

\seealso{\code{\link{createChart}}}

\examples{
test.dataset <- data.frame(First=c(1,1), Second=c(0,1), Third=c(0,0))
createString(test.dataset) # "Abc" "ABc"

# using the column names instead of letters
createString(test.dataset, use.letters=FALSE)


}

\keyword{misc}
