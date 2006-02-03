\name{create.string}

\alias{create.string}

\title{Create a vector of strings containing combinations of presence/absence of conditions}

\description{
The function takes the values from the dataset (0 or 1) and writes the corresponding lower/upper
column names.
}

\usage{
create.string(mydata, use.letters=TRUE)
}

\arguments{
  \item{mydata}{a matrix or a dataframe}
  \item{use.letters}{logical, whether or not to use letters instead of column names}
}

\value{
A list, with:
  \item{comp1:}{a vector of strings, with length equal to the number of rows in \bold{mydata}}
  \item{comp2:}{the separator used to collapse the condition names("" or "*")}
}

\author{
Adrian Dusa\cr
Romanian Social Data Archive, University of Bucharest\cr
adi@roda.ro
}

\examples{
test.dataset <- data.frame(First=c(1,1), Second=c(0,1), Third=c(0,0))
create.string(test.dataset)[[1]] # "Abc" "ABc"

# using the column names instead of letters
create.string(test.dataset, use.letters=FALSE)[[1]] #  "FIRST*second*third" "FIRST*SECOND*third"
}

\keyword{misc}
