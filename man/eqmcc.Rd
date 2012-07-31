\name{eqmcc}

\alias{eqmcc}
\alias{is.qca}

\title{Minimize Canonical Sums using the Enhanced Quine-McCluskey Algorithm}

\description{
This function is the core function of the QCA package, which performs the reduction of
canonical sums to minimal sums. It is called "eqmcc" because it is an 'e'nhancement of 
the classical Quine McCluskey minimization algorithm.
}

\usage{
eqmcc(mydata, outcome = "", neg.out = FALSE, conditions = c(""), n.cut = 1,
      incl.cut1 = 1, incl.cut0 = 1, explain = "1", include = c(""), all.sol = FALSE,
      omit = c(), direxp = c(), rowdom = TRUE, details = FALSE, show.cases = FALSE,
      use.tilde = FALSE, use.letters = FALSE)

is.qca(x)
}

\arguments{
  \item{mydata}{A truth table object or a dataset of (binary or multi-value) crisp or fuzzy-set data}
  \item{outcome}{The name of the outcome set}
  \item{neg.out}{Logical, use negation of outcome set (ignored if \code{mydata} is a truth table object)}
  \item{conditions}{The names of the condition sets (if not specified, all sets in \code{mydata} but the outcome)}
  \item{n.cut}{The minimum number of cases with membership > 0.5 for an outcome value of "0", "1" or "C"}
  \item{incl.cut1}{The minimum sufficiency inclusion score for an outcome value of "1"}
  \item{incl.cut0}{The maximum sufficiency inclusion score for an outcome value of "0"}
  \item{explain}{The outcome value to be explained, either "1", "0" or "C"}
  \item{include}{The outcome value(s) of additional configurations to be included in the minimization}
  \item{all.sol}{Derive all possible solutions, irrespective of the number of PIs}
  \item{omit}{A vector or a matrix of configurations to be omitted from minimization}
  \item{direxp}{A vector of directional expectations}
  \item{rowdom}{Logical, apply row dominance principle to eliminate dominated PIs}
  \item{details}{Logical, present details about solution}
  \item{show.cases}{Logical, also print case names if \code{details = TRUE}}
  \item{use.tilde}{Logical, use a tilde for set negation with binary-value set data}
  \item{use.letters}{Logical, use letters instead of set names}
  \item{x}{An object of class "qca"}
}

\value{An invisible list with the following components:\cr
    \tabular{lll}{
        \tab tt        \tab the truth table object\cr
        \tab excluded  \tab the line numbers of the excluded configurations\cr
        \tab initials  \tab the initial fundamental products be to explained\cr
        \tab PIs       \tab the Prime Implicants, results of the minimization procedure\cr
        \tab PIchart   \tab a list containing the PI chart(s)\cr
        \tab solution  \tab a list of solution(s)\cr
        \tab SA        \tab the Simplifying Assumptions\cr
    }
}

\details{
\code{mydata} can be a truth table object (an object of class "tt" returned by \code{truthTable()}) 
or a dataset of (binary or multi-value) crisp or fuzzy-set data. The dataset specifed as \code{mydata} has to have the following 
structure: values of 0 and 1 for binary-value crisp sets, values between 0 and 1 for fuzzy set data, and values beginning with 0 
and increments of 1 for mutli-value crisp set data. "Don't care" values are indicated by a dash "-" or the placeholder "dc" or any 
negative integer. These values are ignored in the minimization. Sets which contain these values are excluded from the computation 
of parameters of fit.

If the argument \code{conditions} is not specified, all sets in the dataset of (binary or multi-value) crisp 
or fuzzy-set data but the outcome are included.

Configurations that contain fewer than \code{n.cut} cases with membership above 0.5 are coded as logical remainders 
("?"). If the number of such cases is at least \code{n.cut}, configurations with an inclusion score of at least 
\code{incl.cut1} are coded as true ("1"), configuration with an inclusion score below \code{incl.cut1} but with at least 
\code{incl.cut0} are coded as a contradiction ("C"), and configurations with an inclusion score below \code{incl.cut0} 
are coded as false ("0"). If \code{incl.cut0} is not specified, it is set equal to \code{incl.cut1} and no contradictions 
can occur.

The argument \code{explain} specifies the outcome value to be explained. Note that \code{explain = 0} is not the same as
explaining the negation of the outcome, although the solution may be the same, particularly for crisp sets.

The argument \code{include} specifies the outcome values of those configurations which shall be included in addition to
the configurations which are to be explained. Note that argument combinations such as \code{explain = "1", include = "0"}
make no sense.

When \code{rowdom = FALSE}, the argument \code{all.sol} solves the PI chart by deriving all possible solutions, 
irrespective of the number of PIs (suggestion by Michael Baumgartner).

The argument \code{omit} can be used to omit any configuration from the minimization. If the \code{omit} argument is a 
vector, it should contain row numbers from the (complete) truth table. If it is a matrix, it should be of the same format 
as the truth table.

Directional expectations for filtering logical remainders are specified by the \code{direxp} argument. For binary-value 
crisp sets and fuzzy sets they should be given as a vector of the same length and in the same order of conditions as in 
\code{conditions}. A value of "1" indicates that the presence of the condition is expected to contribute to an outcome value
of "1", "0" that the absence of the condition is expected to contribute to an outcome value of "1", and "-1" indicates no 
directional expectation regarding the set-theoretic relation between the configuration and the outcome set. In the case of 
multi-value crisp sets, directional expectations require the identification of the category name(s), separated by semicolons 
and all enclosed by double quotes. The provision of a category name indicates that the presence of this category is expected 
to contribute to an outcome value of "1". Implicitly, it is the absence of all other categories which is expected to 
contribute.

If alternative minimal sums exist, all of them are printed if the row dominance principle for PIs is not applied as specified 
in the logical argument \code{rowdom}. One inessential prime implicant \eqn{P1} dominates another \eqn{P2} if all fundamental 
products covered by \eqn{P2} are also covered by \eqn{P1} and both are not interchangeable. Inessential PIs are listed in 
brackets in the solution output and at the end of the PI part in the parameters-of-fit table when \code{details = TRUE}, 
together with their unique coverage scores under each individual minimal sum.

If the conditions are already named with single letters, the argument \code{use.letters} will have no effect.
}

\references{
A. Dusa. A Mathematical Approach to the Boolean Minimization Problem.
\emph{Quality & Quantity}, 44(1):99-113, 2010.

A. Dusa. \emph{Enhancing Quine-McCluskey}. WP 2007-49, COMPASSS, 2007.\cr
URL: http://www.compasss.org/wpseries/Dusa2007b.pdf.

P. Emmenegger. Job Security Regulations in Western Democracies: A Fuzzy Set Analysis.
\emph{European Journal of Political Research}, 50(3):336-364, 2011.

C. Hartmann and J. Kemmerzell. Understanding Variations in Party Bans in Africa.
\emph{Democratization}, 17(4):642-665, 2010.

M.L. Krook. Women's Representation in Parliament: A Qualitative Comparative Analysis.
\emph{Political Studies}, 58(5):886-908, 2010.

C.C. Ragin. \emph{Redesigning Social Inquiry: Fuzzy Sets and Beyond}. University of Chicago Press, 
Chicago, 2008.

C.C. Ragin and S.I. Strand. Using Qualitative Comparative Analysis to Study Causal Order: Comment on 
Caren and Panofsky (2005). \emph{Sociological Methods & Research}, 36(4):431-441, 2008.
}

\seealso{\code{\link{truthTable}}}

\examples{
# csQCA using Krook (2010)
#-------------------------
data(Krook)
Krook

# explain true configurations, complex solution
eqmcc(Krook, outcome = "WNP")

# explain true configurations with negated outcome set, complex solution
eqmcc(Krook, outcome = "WNP", neg.out = TRUE)

# same result with false configurations (not always the case!)
eqmcc(Krook, outcome = "WNP", explain = "0")

# explain true configurations, parsimonious solution, 
# with solution details
eqmcc(Krook, outcome = "WNP", include = "?", details = TRUE)

# explain true configurations, parsimonious solution, 
# with solution details and without row dominance
KrookSP <- eqmcc(Krook, outcome = "WNP", include = "?", details = TRUE, 
  rowdom = FALSE)
KrookSP
  
# pass truth table object to eqmcc() and derive complex solution
KrookTT <- truthTable(Krook, outcome = "WNP")
KrookSC <- eqmcc(KrookTT)
KrookSC

# print fundamental products
KrookSC$initials

# fsQCA using Emmenegger (2011)
#------------------------------
data(Emme)
Emme

# explain true configurations with negated outcome set, parsimonious solution,
# with solution details
eqmcc(Emme, outcome = "JSR", neg.out = TRUE, include = "?", 
  details = TRUE)

# explain true configurations, intermediate solution, 
# with directional expectations and solution details
EmmeSI <- eqmcc(Emme, outcome = "JSR", incl.cut1 = 0.9, include = "?", 
  direxp = c(1,1,1,1,1,0), details = TRUE)
EmmeSI

# check PI chart for intermediate solution
EmmeSI$PIchart$i.sol

# check simplifying assumptions
EmmeSI$SA$S1

# check easy counterfactuals
EmmeSI$i.sol$C1P1$cntfs

# plot all PIs from intermediate solution
PIsc <- EmmeSI$pims$i.sol$C1P1
par(mfrow = c(2, 2))
for(i in 1:4){
 plot(PIsc[, i], Emme$JSR, pch = 19, ylab = "JSR",
  xlab = colnames(PIsc)[i], xlim = c(0, 1), ylim = c(0, 1),
  main = paste("PI", print(i)))
 abline(0, 1)
}

# mvQCA using Hartmann and Kemmerzell (2010)
#-------------------------------------------
data(HarKem)
HarKem

conds <- c("C", "F", "T", "V")

# explain true configurations, parsimonious solution, 
# with contradictions
HarKemSP <- eqmcc(HarKem, outcome = "PB", conditions = conds, 
  include = c("?", "C"))
HarKemSP

# explain the contradictions
# N.B.: Only one contradiction, no minimization
eqmcc(HarKem, outcome = "PB", conditions = conds, incl.cut0 = 0.4, 
  explain = "C")

# explain true configurations, intermediate solution,
# with directional expectations:
# C{1}, F{1,2}, T{2}, V contribute to PB

HarKemSI <- eqmcc(HarKem, outcome = "PB", conditions = conds, 
  include = "?", direxp = c(1, "1;2", 2, 1))
HarKemSI

# deriving all possible solutions of the PI chart, not just minimal sum

HarKemALL <- eqmcc(HarKem, outcome = "PB", conditions = conds, 
  include = "?", all.sol = TRUE, rowdom = FALSE)
HarKemALL

# tQCA using Ragin and Strand (2008)
#-----------------------------------
data(RagStr)
RagStr

# explain true configurations, complex solution, with solution details and cases;
# auxiliary condition EBA is automatically excluded from parameters of fit
eqmcc(RagStr, outcome = "REC", details = TRUE, show.cases = TRUE)
}

\keyword{functions}


