\name{XYplot}

\alias{XYplot}

\title{Create an XY plot}

\description{
This function creates an XY plot from the first two columns of a dataframe/matrix, or from
two separate vectors of numeric values.
}

\usage{
XYplot(x, y, data, relation = "nec", mguides = TRUE,
       jitter = FALSE, clabels = NULL, ...)
}

\arguments{
  \item{x}{Character, the name of the column from the \bold{\code{data}} for the X axis,
        or the coordinates of points in the plot (either a matrix/dataframe with at least
        two columns, or a vector of values for the X axis).}
  \item{y}{Character, the name of the column from the \bold{\code{data}} for the Y axis,
        or the Y coordinates of points in the plot, optional if \bold{\code{x}}
        is a matrix/dataframe.}
  \item{data}{A calibrated dataset, only if \bold{\code{x}} and \bold{\code{y}} are names.}
  \item{relation}{The set relation to Y, either \bold{\code{"nec"}} (default) or
        \bold{\code{"suf"}}.}
  \item{mguides}{Logical, print the middle guides.}
  \item{jitter}{Logical, jitter the points.}
  \item{clabels}{A vector of case labels with the same length as
        \bold{\code{x}} and \bold{\code{y}}.}
  \item{...}{Other graphical parameters from \code{par}}
}

\details{
If \bold{\code{x}} is a dataframe or a matrix, the X and Y labels will be taken
from the column names of \bold{\code{x}}, otherwise they will be inferred from
the names of the \bold{\code{x}} and \bold{\code{y}} objects that are passed to
this function.

\bold{\code{x}} can also be a string containing either the name of the column
for the X axis, or two column names separated by a comma, referring to the X
and Y axis respectively. When \bold{\code{x}} contains both X and Y column names,
the next argument will be considered as the \bold{\code{data}}.

If \bold{\code{data}} is provided, and the names of the X and Y columns are valid
R statements, quoting them is not even necessary and they can be negated using
either a tilde \bold{\code{~}} or \bold{\code{1 - }}.

The numeric values should be restricted between 0 and 1, otherwise an error is
generated.

The XY plot will also produce inclusion and coverage scores for a sufficiency
or a necessity relation, along with \bold{\code{PRI}} for a sufficiency relation
and \bold{\code{RoN}} (relevance of necessity) for a necessity relation.

The argument \bold{\code{...}} is used to pass various graphical parameters for the
various plotting functions used.

The points have a default \bold{\code{cex}} (character expansion) value of 0.8, and
a default \bold{\code{pch}} value of 21 (filled points), which can be modified
accordingly (for example with value 1 of empty points). When \bold{\code{pch = 21}},
the color for the margins of the points can be specified via the argument
\bold{\code{col}}, while the argument \bold{\code{bg}} will determine the fill color
of the points.

The axis labels have a default \bold{\code{cex.axis}} value of 0.8, which affects
both the tickmarks labels and the axis labels.

When jittering the points, default values of 0.01 are used for the parameters
\bold{\code{factor}} and \bold{\code{amount}}, on both horizontal and vertical axes.
}

\value{
A list of x and y values, especially useful when the points are jittered.
}

\author{
Adrian Dusa
}

\seealso{\code{\link{par}}, \code{\link{text}}, \code{\link{jitter}}}


\references{
Cebotari, V.; Vink, M.P. (2013) \dQuote{A Configurational Analysis of Ethnic
Protest in Europe}. \emph{International Journal of Comparative Sociology}
vol.54, no.4, pp.298-324.

Schneider, C. and Wagemann, C. (2012) \emph{Set-Theoretic Metods for the Social Sciences.
A Guide to Qualitative Comparative Analysis}. Cambridge: Cambridge University Press.
}

\examples{

# Cebotari & Vink (2013)
data(CVF)

# necessity relation between NATPRIDE and PROTEST
XYplot(CVF[, 5:6])

# same using two numeric vectors
XYplot(CVF$NATPRIDE, CVF$PROTEST)

# same using two column names
XYplot("NATPRIDE", "PROTEST", data = CVF)

# or using one string containing both
XYplot("NATPRIDE, PROTEST", data = CVF)

# since they are valid R statements, it works even without quotes
# (this only works in normal R console, not in the GUI version)
XYplot(NATPRIDE, PROTEST, data = CVF)

# negating the X axis, using numeric vectors
XYplot(1 - CVF$NATPRIDE, CVF$PROTEST)

# same thing using quotes
XYplot("1 - NATPRIDE, PROTEST", data = CVF)

# using tilde for negation
XYplot("~NATPRIDE, PROTEST", data = CVF)

# different color for the points
XYplot("~NATPRIDE, PROTEST", data = CVF, col = "blue")

# using a different character expansion for the axes
XYplot("~NATPRIDE, PROTEST", data = CVF, cex.axis = 0.9)

# custom axis labels
XYplot("~NATPRIDE, PROTEST", data = CVF, xlab = "Negation of NATPRIDE",
       ylab = "Outcome: PROTEST")

# sufficiency relation
XYplot("~NATPRIDE, PROTEST", data = CVF, relation = "suf")

# jitter the points
XYplot("~NATPRIDE, PROTEST", data = CVF, jitter = TRUE)

# jitter with more amount
XYplot("~NATPRIDE, PROTEST", data = CVF, jitter = TRUE, amount = 0.02)

# adding labels to points
XYplot("~NATPRIDE, PROTEST", data = CVF, jitter = TRUE, cex = 0.8,
       clabels = rownames(CVF))

# or just the row numbers, since the row names are too long
XYplot("~NATPRIDE, PROTEST", data = CVF, jitter = TRUE, cex = 0.8,
       clabels = seq(nrow(CVF)))

}


\keyword{functions}
