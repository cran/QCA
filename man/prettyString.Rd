\name{prettyString}

\alias{prettyString}

\title{Separate a very long vector of strings on several rows}

\description{
  When displaying the minimized prime implicants or the row names from the original dataset,
  it sometimes happens that this vector is too large to be displayed on a single row, therefore
  R splitts it at some letter when printing it. This function takes care of that, by specifying
  the maximum number of characters for each row such that no prime implicants or
  row names will be splitted.
}

\usage{
prettyString(string.vector, string.width=60, repeat.space=5,
              separator=",")
}

\arguments{
  \item{string.vector}{a vector of strings}
  \item{string.width}{the length of the row, in number of characters (default to 60)}
  \item{repeat.space}{the number of space characters for the white space from the beginning of the line (default to 5)}
  \item{separator}{character to use for separating the vector elements (default to comma)}
}

\author{
Adrian Dusa\cr
Romanian Social Data Archive\cr
adi@roda.ro\cr
Faculty of Sociology and Social Work, University of Bucharest\cr
adi@sas.unibuc.ro
}

\seealso{\code{\link[base]{cat}}, \code{\link[base]{strsplit}}, \code{\link[base]{strwrap}}}

\examples{
set.seed(1234)
long.string <- paste("This is a very long string, composed from the following words:", 
                 paste(sample(c("one","two","three"), 60, replace=TRUE), collapse=" "))

long.string

string.vector <- unlist(strsplit(long.string, " "))

# a row length of 70, no space at the beginning
cat(prettyString(string.vector, 70, 0, " "), "\n")

# a row length of 50, inserting 7 spaces before each row
cat("      ", prettyString(string.vector, 50, 7, " "), "\n")


}

\keyword{misc}
