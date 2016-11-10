\name{export}

\alias{export}

\title{Export a dataframe to a file or a connection}

\description{
This function is a wrapper to \bold{\code{write.table()}}, see the details section.
}

\usage{
export(x, file = "", ...)
}

\arguments{
    \item{x}{The object to be written}
    \item{file}{A character string containing the path to the file to be created}
    \item{...}{Same arguments that are used in \bold{\code{write.table()}}}
}

\details{
The default convention for \bold{\code{write.table()}} is to add a blank column name for the row names,
but (despite it is the convention used for CSV files to be read by spreadsheets) that doesn't
always work with all spreadsheets or other programs that attempt to import the result of
\bold{\code{write.table()}}.

This function acts as if \bold{\code{write.table()}} was called, with only one difference: if row names
are present in the dataframe (i.e. any of them should be different from the default row numbers),
the final result will display a new column called \bold{\code{cases}} in the first position, except the
situation that another column called \bold{\code{cases}} already exists in the data, when the row names
will be completely ignored.

If not otherwise specified, an argument \bold{\code{sep = ","}} is added by default.

The argument \bold{\code{row.names}} is set to FALSE by default, since a new column is added anyways if possible.

Since this function pipes everything to \bold{\code{write.table()}} anyway, the argument \bold{\code{file}}
can also be a connection open for writing. \bold{\code{""}} indicates output to the console.
}

\author{
Adrian Dusa
}


\seealso{
  The \dQuote{R Data Import/Export} manual.

  \code{\link{write.table}}
}

\keyword{functions}
