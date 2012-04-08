\name{pof}

\alias{pof}

\title{Set-Relational Parameters of Fit}

\description{
This function computes three set-relational parameters of fit: inclusion, PRI and (raw) coverage scores.
}

\usage{
pof(setms, mydata, outcome = "", relation = "necessity", neg.out = FALSE, ...)
}

\arguments{
  \item{setms}{Set membership scores, or a matrix with crisp set representations, or a vector of line numbers}
  \item{mydata}{The original dataset}
  \item{outcome}{The name of the outcome set}
  \item{relation}{The set relation of the conditions to the outcome, either "necessity" or "sufficiency"}
  \item{neg.out}{Logical, use negation of outcome set}
  \item{...}{Other arguments from the generic print (not used in this function)}
}

\details{
  The argument \code{setms} specifies a dataframe of \emph{set} membership scores, where \emph{set}
  refers to any kind of set, including original condition sets, conjunctive or disjunctive combinations returned 
  by \code{superSubset()}, prime implicants returned by \code{eqmcc()} or any other compound set.
  
  It can also be a matrix with the crisp set representation of the causal conditions, or even a vector
  of line numbers corresponding to those representations.
}

\references{

P. Emmenegger. Job Security Regulations in Western Democracies: A Fuzzy Set Analysis.
\emph{European Journal of Political Research}, 50(3):336-364, 2011.

M. L. Krook. Women's Representation in Parliament: A Qualitative Comparative Analysis.
\emph{Political Studies}, 58(5):886-908, 2010.

}

\examples{
# csQCA using Krook (2010)
#-------------------------
data(Krook)
Krook
x <- Krook[,1:5]
 
# get necessity parameters of fit for all conditions
pof(x, Krook, outcome = "WNP")

# now for the negated outcome
pof(x, Krook, outcome = "WNP", neg.out = TRUE)

# get sufficiency parameters of fit for all original conditions
pof(x, Krook, outcome = "WNP", relation = "sufficiency") 

# now for the negated outcome
pof(x, Krook, outcome = "WNP", relation = "sufficiency", neg.out = TRUE)

# fsQCA using Emmenegger (2011)
#------------------------------
data(Emme)
Emme

# first test for necessary conditions with superSubset(), then 
# check whether the returned combinations are also necessary for 
# the negation of the outcome

EmmeNR <- superSubset(Emme, outcome = "JSR", incl.cut = 0.965, cov.cut = 0.6)
EmmeNR
pof(EmmeNR$coms, Emme, outcome = "JSR", neg.out = TRUE) 

# first derive the complex solution, then check whether the negations 
# of the prime implicants are also sufficient for the outcome 

EmmeSC <- eqmcc(Emme, outcome = "JSR", incl.cut1 = 0.9, details = TRUE)
EmmeSC
pof(1 - EmmeSC$pims$c.sol, Emme, outcome = "JSR", relation = "sufficiency")

# parameters of fit for any term, including configurations;
# use "-1" as a placeholder for a minimized literal; 
#     [,1] [,2] [,3] [,4] [,5] [,6]
#[1,]   -1   -1   -1    1    0    1
#[2,]    1    0    1   -1    1    0

confs <- matrix(c(-1, -1, -1,  1, 0, 1, 
                   1,  0,  1, -1, 1, 0), nrow = 2, byrow = TRUE)
confs
pof(confs, Emme, outcome = "JSR", relation = "sufficiency")

# or even vectors of line numbers from the n^k matrix
pof(c(43, 57), Emme, "JSR", relation = "sufficiency")

# in this case, the line numbers 43 and 57 represent the following
# terms / configurations:
#     [,1] [,2] [,3] [,4] [,5] [,6]
#[1,]   -1   -1    0    0    1   -1   -> l*r*P
#[2,]   -1   -1    1   -1   -1    1   -> L*V

getRow(rep(3, 6), c(43, 57)) - 1

}

\keyword{functions}
