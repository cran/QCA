\name{pretty.string}

\alias{pretty.string}

\title{Separate a very long vector of strings on several rows}

\description{
  When displaying the minimized prime implicants or the row names from the original dataset,
  it sometimes happens that this vector is too large to be displayed on a single row, therefore
  R splitts it at some letter when printing it. This function takes care of that, by specifying
  the maximum number of characters for each row such that no prime implicants or
  row names will be splitted.
}

\usage{
pretty.string(string.vector, string.width, repeat.space, separator)
}

\arguments{
  \item{string.vector}{a vector of strings}
  \item{string.width}{the length of the row, in number of characters}
  \item{repeat.space}{the number of space characters for the white space from the beginning of the line}
  \item{separator}{character to use for separating the vector elements}
}

\author{
Adrian Dusa\cr
Romanian Social Data Archive, University of Bucharest\cr
adi@roda.ro
}

\seealso{cat, strsplit}

\examples{
string.vector <- unlist(strsplit("When displaying the minimized prime implicants or
  the row names from the original dataset, it sometimes happens that this vector is
  too large to be displayed on a single row, therefore R splitts it at some letter when
  printing it. This function takes care of that, by specifying the maximum number of
  characters for each row such that no prime implicants or   row names will be splitted.",
  " "))

cat(pretty.string(string.vector, 50, 10, ";"), "\n")

cat(pretty.string(string.vector, 70, 5, ";"), "\n")
}

\keyword{misc}
