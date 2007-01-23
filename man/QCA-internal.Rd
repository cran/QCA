\name{QCA-internal}

\alias{sortVector}
\alias{sortMatrix}
\alias{writePrimeimp}
\alias{writeSolution}
\alias{verify.qmcc}
\alias{verify.truthTable}
\alias{createChart}
\alias{rowDominance}


\title{Internal Functions}

\description{
  \code{sortVector}, \code{sortMatrix}, \code{writePrimeimp} and \code{writeSolution} are internal functions of the 
  QCA package which are not designed to be called directly by the user. All of them are used by 
  the main \code{qmcc} function, except \code{sortMatrix} which is used by \code{allExpressions}.
  The verification and error messages have been moved to the internal functions
  \code{verify.qmcc} and \code{verify.truthTable}.
}

\keyword{internal}

