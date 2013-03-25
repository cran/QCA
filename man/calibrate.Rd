\name{calibrate}

\alias{calibrate}

\title{Calibrate Crisp (Binary or Multi-Value) and Fuzzy Sets}

\description{
This function produces crisp and fuzzy sets from \dfn{raw data} (base variables) and some specified threshold(s).
The calibration of fuzzy sets is possible for end-point and mid-point concepts,
using the method of transformational assignment.
}

\usage{
calibrate(x, type = "crisp", thresholds = NA, include = TRUE, logistic = FALSE,
          idm = 0.95, ecdf = FALSE, p = 1, q = 1)
}

\arguments{
  \item{x}{An interval or ratio-scaled base variable.}
  \item{type}{The calibration type, either "crisp" or "fuzzy".}
  \item{thresholds}{A vector of thresholds.}
  \item{include}{Logical, include threshold(s) in the set (\code{type = "crisp"} only).}
  \item{logistic}{Calibrate to fuzzy sets using the logistic function.}
  \item{idm}{The set inclusion degree of membership for the logistic function.}
  \item{ecdf}{Calibrate to fuzzy sets using the empirical cumulative distribution function of the base variable.}
  \item{p}{Parameter: if p > 1 concentration, if 0 < p < 1 dilation below crossover.}
  \item{q}{Parameter: if q > 1 dilation, if 0 < q < 1 concentration above crossover.}
}

\details{
Calibration is the process by which set membership scores are assigned to cases.
With interval and ratio-scaled base variables, calibration can be based on
transformational assignments using (piecewise-defined) membership functions.

For \code{type = "crisp"}, one threshold produces crisp sets with two groups - 0s and 1s.
More thresholds produce multi-value data. For example, two thresholds produce three 
groups - 0s, 1s and 2s.

For \code{type = "fuzzy"}, this function can generate linear, s-shaped and inverted s-shaped 
fuzzy numbers with respect to end-point concepts, including logistic transformations.
It can generate trapezoidal, triangular and bell-shaped fuzzy numbers with respect to mid-point concepts 
(Bojadziev and Bojadziev, 2007; Clark et al., 2008; Thiem and Dusa, 2013).

For calibrating fuzzy sets based on end-point concepts, thresholds should be specified as
a numeric vector \code{thresholds = c(thEX, thCR, thIN)}, where \code{thEX} is the threshold for 
full set exclusion, \code{thCR} the threshold for set crossover, and \code{thIN} the 
threshold for full set inclusion.

If \code{thEX < thCR < thIN}, then the membership function is increasing from \code{thEX} to \code{thIN}.\cr
If \code{thIN < thCR < thEX}, then the membership function is decreasing from \code{thIN} to \code{thEX}.

For calibrating fuzzy sets based on mid-point concepts, thresholds should be specified as 
a vector \code{thresholds = c(thEX1, thCR1, thIN1, thIN2, thCR2, thEX2)}, where \code{thEX1} is the 
first (left) threshold for full set exclusion, \code{thCR1} the first (left) threshold for set crossover, 
\code{thIN1} the first (left) threshold for full set inclusion, \code{thIN2} the second (right) threshold 
for full set inclusion, \code{thCR2} the second (right) threshold for set crossover, and \code{thEX2} 
the second (right) threshold for full set exclusion.

If \code{thEX1 < thCR1 < thIN1 <= thIN2 < thCR2 < thEX2}, then the membership function is first increasing 
from \code{thEX1} to \code{thIN1}, then decreasing from \code{thIN2} to \code{thEX2}.\cr
If \code{thIN1 < thCR1 < thEX1 <= thEX2 < thCR2 < thIN2}, then the membership function is first decreasing
from \code{thIN1} to \code{thEX1}, then increasing from \code{thEX2} to \code{thIN2}.

The parameters \code{p} and \code{q} control the degree of concentration and dilation. They should be left at 
their default values unless good reasons to change them exist.

If \code{logistic = TRUE}, the argument \code{idm} specifies the inclusion degree of membership in the set.
The exclusion degree is automatically determined because of the symmetry of the logistic function.

If \code{ecdf = TRUE}, calibration is based on the empirical cumulative distribution function of \code{x}. The 
arguments \code{logistic} and \code{ecdf} are mutually exclusive, with the first taking precedence over the second.
}

\value{
A numeric vector of crisp set or fuzzy set membership scores.
}

\references{
G. Bojadziev and M. Bojadziev. \emph{Fuzzy Logic for Business, Finance, and 
Management}. 2nd ed., World Scientific, New Jersey, 2007.

T. D. Clark, J. M. Larson, J. N. Mordeson, J. D. Potter, and M. J. Wierman. 
\emph{Applying Fuzzy Mathematics to Formal Models in Comparative Politics}. 
Springer, Berlin, 2008.

A. Thiem and A. Dusa. \emph{Qualitative Comparative Analysis with R: A User's Guide}. 
Springer, New York, 2013.
}

\examples{
# base variable; random draw from standard normal distribution
x <- rnorm(30)

# calibration thresholds
th <- quantile(x, seq(from = 0.1, to = 0.9, length = 5))

# calibration of binary-value crisp sets (two groups)
calibrate(x, thresholds = th[3])

# calibration of multi-value crisp set (three groups)
calibrate(x, thresholds = c(th[2], th[4]))

# fuzzy-set calibration (positive end-point concept, linear)
plot(x, calibrate(x, type = "fuzzy", thresholds = c(th[1], th[3], th[5])), 
  ylab = "Fuzzy Set Membership")

# fuzzy-set calibration (positive end-point concept, logistic)
plot(x, calibrate(x, type = "fuzzy", thresholds = c(th[1], th[3], th[5]), 
  logistic = TRUE, idm = 0.99), ylab = "Fuzzy Set Membership")

# fuzzy-set calibration (positive end-point concept, ECDF)
plot(x, calibrate(x, type = "fuzzy", thresholds = c(th[1], th[3], th[5]), 
  ecdf = TRUE), ylab = "Fuzzy Set Membership")

# fuzzy-set calibration (negative end-point concept, s-shaped)
plot(x, calibrate(x, type = "fuzzy", thresholds = c(th[5], th[3], th[1]), 
  p = 2, q = 2), ylab = "Fuzzy Set Membership")

# fuzzy-set calibration (positive mid-point concept, triangular)
plot(x, calibrate(x, type = "fuzzy", thresholds = th[c(1,2,3,3,4,5)]),
  ylab = "Fuzzy Set Membership")

# fuzzy-set calibration (negative mid-point concept, bell-shaped)
plot(x, calibrate(x, type = "fuzzy", thresholds = th[c(3,2,1,5,4,3)],
  p = 3, q = 3), ylab = "Fuzzy Set Membership")

}

\keyword{functions}
