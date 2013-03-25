\name{truthTable}
\alias{truthTable}
\alias{is.tt}

\title{Create a Truth Table}

\description{
A truth table lists every \dfn{configuration} (combination of condition literals) 
and its outcome value. This function creates truth tables from any kind of common 
input data. For crisp-set data (binary and multi-value), cases are assigned directly 
to the unique configuration in which they have membership. For fuzzy-set data, the 
function first detects in which configuration a case has membership above 0.5 before 
assigning it to this configuration.\cr
\cr
\code{is.tt()} tests whether an object has class "tt" (a truth table created by 
\code{truthTable()}
}

\usage{
truthTable(data, outcome = "", neg.out = FALSE, conditions = c(""), n.cut = 1,
           incl.cut1 = 1, incl.cut0 = 1, complete = FALSE, show.cases = FALSE,
           sort.by = c(""), decreasing = TRUE, use.letters = FALSE, ...)

is.tt(x)
}

\arguments{
  \item{data}{A dataset of (binary or multi-value) crisp or fuzzy-set data.}
  \item{outcome}{The name of the outcome set.}
  \item{neg.out}{Logical, use negation of outcome set.}
  \item{conditions}{The names of the conditions.}
  \item{n.cut}{The minimum number of cases with membership > 0.5 for an outcome value of "0", "1" or "C".}
  \item{incl.cut1}{The minimum sufficiency inclusion score for an outcome value of "1".}
  \item{incl.cut0}{The maximum sufficiency inclusion score for an outcome value of "0".}
  \item{complete}{Logical, print complete truth table.}
  \item{show.cases}{Logical, print case names.}
  \item{sort.by}{Sort truth table by inclusion scores and/or number of cases.}
  \item{decreasing}{Sort in decreasing or increasing order of value(s) passed to \code{sort.by}.}
  \item{use.letters}{Logical, use letters instead of set names.}
  \item{x}{An object of class "tt".}
  \item{...}{Other arguments (used internally).}
}

\value{An object of class "tt", which is a list with the following components:\cr
  \item{tt}{The truth table.}
  \item{indexes}{The configuration line numbers.}
  \item{noflevels}{A vector with the number of condition levels.}
  \item{initial.data}{The initial data.}
  \item{recoded.data}{Recoded data (if crisp, same as \code{initial.data}; if fuzzy, 
                      dichotomized version of \code{initial.data}).}
  \item{cases}{The cases with membership above 0.5 in each configuration.}
}

\details{
The dataset specifed as \code{mydata} has to have the following structure: values of 0 and 1 for binary-value crisp sets,
values between 0 and 1 for fuzzy set data, and values beginning with 0 and increments of 1 for mutli-value crisp set data.
"Don't care" values are indicated by a dash "-", the placeholder "dc" or any negative integer. These values are ignored 
in the minimization. Sets which contain these values are excluded from the computation of parameters of fit.  

If the argument \code{conditions} is not specified, all sets in \code{mydata} but the outcome are included 
as conditions.

Configurations that contain fewer than \code{n.cut} cases with membership above 0.5 are coded as logical remainders 
("?"). If the number of such cases is at least \code{n.cut}, configurations with an inclusion score of at least 
\code{incl.cut1} are coded as true ("1"), configuration with an inclusion score below \code{incl.cut1} but with at least 
\code{incl.cut0} are coded as a contradiction ("C"), and configurations with an inclusion score below \code{incl.cut0} 
are coded as false ("0"). If \code{incl.cut0} is not specified, it is set equal to \code{incl.cut1} and no contradictions 
can occur.

The \code{sort.by} argument orders all configurations by their inclusion scores (\emph{incl}) or the number of
cases with membership above 0.5 they contain (\emph{n}) or both, in either order.

If the conditions are already named with single letters, the argument \code{use.letters} will have no effect.
}

\references{
P. Emmenegger. Job Security Regulations in Western Democracies: A Fuzzy Set Analysis.
\emph{European Journal of Political Research}, 50(3):336-364, 2011.

C. Hartmann and J. Kemmerzell. Understanding Variations in Party Bans in Africa.
\emph{Democratization}, 17(4):642-665, 2010.

M. L. Krook. Women's Representation in Parliament: A Qualitative Comparative Analysis.
\emph{Political Studies}, 58(5):886-908, 2010.

C. C. Ragin. \emph{The Comparative Method: Moving beyond Qualitative and Quantitative Strategies}. 
University of California Press, Berkeley, 1987.

C. C. Ragin. \emph{Redesigning Social Inquiry: Fuzzy Sets and Beyond}. University of Chicago Press, 
Chicago, 2008.

C. C. Ragin and S. I. Strand. Using Qualitative Comparative Analysis to Study Causal Order: Comment on Caren and Panofsky (2005).
\emph{Sociological Methods & Research}, 36(4):431-441, 2008.

A. Thiem and A. Dusa. \emph{Qualitative Comparative Analysis with R: A User's Guide}. 
Springer, New York, 2013.

}

\seealso{\code{\link{eqmcc}}}

\examples{
# csQCA using Krook (2010)
#-------------------------
data(Krook)
Krook

# print the truth table
truthTable(Krook, outcome = "WNP")

# print the complete truth table, show cases, sort by inclusions
# scores and then number of cases
truthTable(Krook, outcome = "WNP", complete = TRUE, show.cases = TRUE,
  sort.by = c("incl", "n"))

# code configurations with 1 case as remainders
KrookTT <- truthTable(Krook, outcome = "WNP", n.cut = 2, show.cases = TRUE)
KrookTT

# print the cases that were assigned to remainders based on n.cut
KrookTT$excluded

# fsQCA using Emmenegger (2011)
#------------------------------
data(Emme)
Emme

# code non-remainder configurations with inclusion scores between 0.5 
# and 0.9 as contradictions
EmmeTT <- truthTable(Emme, outcome = "JSR", incl.cut1 = 0.9, incl.cut0 = 0.5)
EmmeTT

# truth table based on negated outcome set
EmmeTT <- truthTable(Emme, outcome = "JSR", neg.out = TRUE, incl.cut1 = 0.9, 
  incl.cut0 = 0.5)
EmmeTT

# get the raw truth table as a component of the truth table object EmmeTT
EmmeTT$tt 

# mvQCA using Hartmann and Kemmerzell (2010)
#-------------------------------------------
data(HarKem)
HarKem

# code non-remainder configurations with inclusion scores below 1 but above
# 0.4 as contradictions 
HarKemTT <- truthTable(HarKem, outcome = "PB", 
  conditions = c("C","F","T","V"), incl.cut0 = 0.4)
HarKemTT

# list the number of condition levels  
HarKemTT$noflevels

# which configurations have more than 2 cases
which(HarKemTT$tt$n > 2)

# tQCA using Ragin and Strand (2008)
#-----------------------------------
data(RagStr)
RagStr

# tQCA truth table with "don't care" values
truthTable(RagStr, outcome = "REC")

}

\keyword{functions}
