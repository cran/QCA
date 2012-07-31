\name{QCA-internal}

\alias{base3rows}
\alias{createChart}
\alias{createString}
\alias{prettyString}
\alias{prettyTable}
\alias{print.aE}
\alias{print.fctr}
\alias{print.pic}
\alias{print.pof}
\alias{print.qca}
\alias{print.ss}
\alias{print.tt}
\alias{rowDominance}
\alias{rowDominance2}
\alias{sortVector}
\alias{sortMatrix}
\alias{verify.data}
\alias{verify.expl}
\alias{verify.qca}
\alias{verify.tt}
\alias{writePrimeimp}
\alias{writeSolution}


\title{Internal Functions}

\description{
  \code{base3rows}, \code{createChart}, \code{createString}, \code{prettyString},
  \code{prettyTable}, \code{rowDominance}, \code{rowDominance2}, \code{sortVector},
  \code{sortMatrix}, \code{writePrimeimp} and \code{writeSolution}
  are internal functions of the QCA package which are not designed to be called directly
  by the user. All of them are used by the \code{eqmcc} function, except
  \code{sortMatrix} which is used by \code{allExpressions}. The verification and error
  messages have been moved to the internal functions \code{verify.data} and \code{verify.tt}.
}

\keyword{internal}

