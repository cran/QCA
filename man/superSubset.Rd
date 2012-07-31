\name{superSubset}
\alias{superSubset}

\title{Find Superset/Subset Relations}

\description{
This function finds all conjunctive/disjunctive combinations of conditions among all possible such 
combinations which optimize the fulfilment of the specified criteria set for a superset (necessity) / 
subset (sufficiency) relation to the outcome and are minimally complex. 
}

\usage{
superSubset(mydata, outcome = "", neg.out = FALSE, conditions = c(""), 
            relation = "necessity", incl.cut = 1, cov.cut = 0, use.tilde = FALSE,
            use.letters = FALSE)
}

\arguments{
  \item{mydata}{A dataset of (binary or multi-value) crisp or fuzzy-set data}
  \item{outcome}{The name of the outcome set}
  \item{neg.out}{Logical, use negation of outcome set}
  \item{conditions}{The names of the condition}
  \item{relation}{The set relation of the conditions to the outcome, either "necessity" or "sufficiency"}
  \item{incl.cut}{The minimal inclusion score}
  \item{cov.cut}{The minimal coverage score}
  \item{use.tilde}{Logical, use a tilde for set negation}
  \item{use.letters}{Logical, use letters instead of set names}
}

\value{An object of class "ss", which is a list with the following components:\cr
    \tabular{ll}{
      $incl.cov: \tab a dataframe with the parameters of fit\cr
      $coms:     \tab a dataframe with the combination membership scores  
    }
}

\details{
This function returns a list of those combinations of the \eqn{3^k - 1} potential combinations of \eqn{k} conditions 
which are minimally complex, yet exceed the given cut-offs for inclusion and coverage scores. In addition to inclusion 
and coverage scores, PRI (proportional reduction in inconsistency) scores are also returned. The following paragraphs 
describe the theoretical idea behind the function. Computionally, the algorithm implementation is more efficient.

If \code{relation = "necessity"}, the initial set of combinations is comprised of the \eqn{2k} order-1 combinations as well 
as their negations. This set is expanded incrementally by forming conjunctive combinations of a higher order as long as 
the cut-offs are still met. The inclusion cut-off always enjoys a higher priority than the coverage cut-off. All resulting 
combinations as well as all subcombinations of a lower order will be returned.

If none of the initial order-1 combinations passes the inclusion cut-off, \code{superSubset()} will search for disjunctive 
combinations until the cut-offs have been met. Only the disjunctive combinations thus found will be returned.

If \code{relation = "sufficiency"}, the initial set of combinations is comprised of all order-\eqn{k} conjunctive combinations. 
This set is reduced incrementally by forming conjunctive combinations of a lower order as long as the cut-offs are still met. 
Only the conjunctive combinations of the lowest order will be printed. Disjunctive combinations are not formed. For more details, 
see Thiem and Dusa (2012).

The argument \code{use.tilde} only applies to non-multi-value data. If the conditions are already named with single letters, 
the argument \code{use.letters} will have no effect.
}
    
\references{
P. Emmenegger. Job Security Regulations in Western Democracies: A Fuzzy Set Analysis.
\emph{European Journal of Political Research}, 50(3):336-364, 2011.

M. L. Krook. Women's Representation in Parliament: A Qualitative Comparative Analysis.
\emph{Political Studies}, 58(5):886-908, 2010.

A. Thiem and A. Dusa. \emph{Qualitative Comparative Analysis with R: A User's Guide}. 
Springer, New York, 2012.    
}

\examples{
# csQCA using Krook (2010)
#-------------------------
data(Krook)
Krook

# find all optimal combinations with a necessity inclusion score 
# of at least 0.9 and a necessity coverage score of at least 0.6 
KrookSS <- superSubset(Krook, outcome = "WNP", incl.cut = 0.9, cov.cut = 0.6)
KrookSS

# the combination membership scores for all cases
KrookSS$coms

# mvQCA using Hartmann and Kemmerzell (2010)
#-------------------------------------------
data(HarKem)
HarKem

# find all optimal combinations with perfect necessity inclusion
HarKemSS <- superSubset(HarKem, outcome = "PB", conditions = c("C", "F", "T", "V"))
HarKemSS

# the combination membership scores for all cases
HarKemSS$coms

# fsQCA using Emmenegger (2011)
#------------------------------
data(Emme)
Emme

# find all optimal combinations with a sufficiency inclusion score 
# of at least 0.9 and sufficiency coverage score of at least 0.4 
# N.B.: Some combinations may appear as solution PIs.
EmmeSS <- superSubset(Emme, outcome = "JSR", relation = "sufficiency", 
  incl.cut = 0.9, cov.cut = 0.4)
EmmeSS

# the same criteria, but for the negation of the outcome
EmmeSSN <- superSubset(Emme, outcome = "JSR", neg.out = TRUE, 
  relation = "sufficiency", incl.cut = 0.9, cov.cut = 0.4)
EmmeSSN

# the combination membership scores for all cases
EmmeSSN$coms
}

\keyword{functions}
