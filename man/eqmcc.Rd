\name{eqmcc}

\alias{eqmcc}

\title{QCA minimization using the enhanced Quine-McCluskey algorithm}

\description{
This function performs the QCA minimization of some causal conditions, with
respect to an outcome. It is called \dQuote{eqmcc} from the (e)nhanced
Quine-McCluskey, a different algorithm which returns the same, exact solutions.
}

\usage{
eqmcc(data, outcome = "", conditions = "",  relation = "suf", n.cut = 1,
      incl.cut = 1,  explain = "1", include = "", row.dom = FALSE, all.sol = FALSE,
      omit = NULL, dir.exp = "", details = FALSE, show.cases = FALSE,
      inf.test = "", use.tilde = FALSE, use.letters = FALSE, ...)
}

\arguments{
  \item{data}{A truth table object or a data frame containing calibrated causal
        conditions and an outcome.}
  \item{outcome}{A string containing the outcome(s) name(s), separated
        by commas.}
  \item{conditions}{A single string containing the conditions' (columns) names
        separated by commas, or a character vector of conditions' names.}
  \item{relation}{The set relation to \bold{\code{outcome}}, either
        \bold{\code{"suf"}} or \bold{\code{"sufnec"}}.}
  \item{n.cut}{The minimum number of cases under which a truth table row is 
        declared as a remainder.}
  \item{incl.cut}{The inclusion cutoff(s): either a single value for the presence
        of the output, or a vector of length 2, the second for the absence of the
        output.}
  \item{explain}{A vector of output values to explain.}
  \item{include}{A vector of other output values to include in the minimization
        process.}
  \item{row.dom}{Logical, perform row dominance in the prime implicants' chart to
        eliminate redundant prime implicants.}
  \item{all.sol}{Derive all possible solutions, irrespective of the number of prime
        implicants.}
  \item{omit}{A vector of row numbers from the truth table, or a matrix of causal 
        combinations to omit from the minimization process.}
  \item{dir.exp}{A vector of directional expectations to derive intermediate
        solutions.}
  \item{details}{Logical, print more details about the solution.}
  \item{show.cases}{Logical, print case names.}
  \item{inf.test}{Specifies the statistical inference test to be performed
        (currently only \bold{\code{"binom"}}) and the critical significance level.
        It can be either a vector of length 2, or a single string containing both,
        separated by a comma.}
  \item{use.tilde}{Logical, use tilde for negation with bivalent variables.}
  \item{use.letters}{Logical, use letters instead of causal conditions' names.}
  \item{...}{Other arguments (mainly for backwards compatibility).}
}

\details{
The argument \bold{\code{data}} can be either a truth table object (created with the
function \bold{\code{truthTable()}}) or a data frame containing calibrated columns.

Calibration can be either crisp, with 2 or more values starting from 0, or fuzzy with
continous scores from 0 to 1. Raw data containing relative frequencies can
also be continous between 0 and 1, but these are not calibrated, fuzzy data.

Some columns can contain the placeholder \bold{\code{"-"}} indicating a
\dQuote{don't care}, which is used to indicate the temporal order between other
columns in tQCA. These special columns are not causal conditions, hence no parameters
of fit will be calculated for them.

The argument \bold{\code{outcome}} specifies the column name to be explained.
If the outcome is a multivalue column, it can be specified in curly bracket notation,
indicating the value to be explained (the others being automatically converted to
zero).

The outcome can be negated using a tilde operator \bold{\code{~X}}. The logical
argument \bold{\code{neg.out}} is now deprecated, but still backwards compatible.
Replaced by the tilde in front of the outcome name, it controls whether
\bold{\code{outcome}} is to be explained or its negation.

If the outcome column is multi-value, the argument \bold{\code{outcome}} should use
the standard curly-bracket notation \bold{\code{X{value}}}. Multiple values are
allowed, separated by a comma (for example \bold{\code{X{1,2}}}). Negation of the
outcome can also be performed using the tilde \bold{\code{~}} operator, for example
\bold{\code{~X{1,2}}}, which is interpreted as: "all values in X except 1 and 2"
and it becomes the new outcome to be explained.

Using both \bold{\code{neg.out = TRUE}} and a tilde \bold{\code{~}} in the outcome
name don't cancel each other out, either one (or even both) signaling if the
\bold{\code{outcome}} should be negated.

This function supports multiple outcomes, in which case all of them should also be
specified in the argument \bold{\code{conditions}}.

The argument \bold{\code{conditions}} specifies the causal conditions' names among
the other columns in the data.  For backwards compatibility, this argument also
accepts a character vector of condition variables' names. When this argument is not
specified, all other columns except for the outcome are taken as causal conditions
(and in case there are multiple outcomes, all columns are considered causal
conditions).

A good practice advice is to specify both \bold{\code{outcome}} and
\bold{\code{conditions}} as upper case letters. It is possible, in a next version,
to negate outcomes using lower case letters, situation in which it really does
matter how the outcome and/or conditions are specified.

The argument \bold{\code{relation}} is used to identify solutions which are
sufficient for the outcome. When using \bold{\code{relation = "suf"}}, the function
will return all solutions which are sufficient for the outcome (whether necessary
or not). If using \bold{\code{relation = "sufnec"}}, only those solutions which are
both sufficient and necessary will be returned.

The argument \bold{\code{n.cut}} specifies the frequency threshold under which a
truth table row is coded as a remainder, irrespective of its inclusion score.

The argument \bold{\code{incl.cut}} replaces the (deprecated, but still backwards
compatible) former arguments \bold{\code{incl.cut1}} and \bold{\code{incl.cut0}}.
Most of the analyses use the inclusion cutoff for the presence of the output
(code \bold{\code{"1"}}). When users need both inclusion cutoffs (see below),
\bold{\code{incl.cut}} can be specified as a vector of length 2, in the form:
\bold{\code{c(ic1, ic0)}} where:

\tabular{rl}{
\bold{\code{ic1}} \tab is the inclusion cutoff for the presence of the output,\cr
                  \tab a minimum sufficiency inclusion score above which the output
                       value is coded with \code{"1"}.\cr \cr
\bold{\code{ic0}} \tab is the inclusion cutoff for the absence of the output,\cr
                  \tab a maximum sufficiency inclusion score below which the output
                       value is coded with \code{"0"}.\cr
}

If not specifically declared, the argument \bold{\code{ic0}} is automatically set
equal to \bold{\code{ic1}}, but otherwise \bold{\code{ic0}} should always be lower
than \bold{\code{ic1}}.

Using these two cutoffs, the observed combinations are coded with:

\tabular{rl}{
\bold{\code{"1"}} \tab if they have an inclusion score above \bold{\code{ic1}}\cr \cr
\bold{\code{"C"}} \tab if they have an inclusion score below \bold{\code{ic1}} and
above \bold{\code{ic0}} (contradiction)\cr \cr
\bold{\code{"0"}} \tab if they have an inclusion score below \bold{\code{ic0}}\cr
}

The argument \bold{\code{explain}} specifies the output values corresponding to the
truth table rows which enter in the minimization process.
Such values can be \bold{\code{"1"}}, \bold{\code{"C"}}, \bold{\code{"0"}},
\bold{\code{"1, C"}} and \bold{\code{"0, C"}}, but not \bold{\code{"1, 0"}} and
\bold{\code{"1, 0, C"}}. Note that for \bold{\code{"0"}}, \bold{\code{"C"}} and
\bold{\code{"0, C"}}, configurations will be reduced but no solution details
printed.

The argument \bold{\code{include}} specifies which other truth table rows are
included in the minimization process. Most often, the remainders are included but
any value accepted in the argument \bold{\code{explain}} is also accepted in the
argument \bold{\code{include}}.

The argument \bold{\code{row.dom}} is used to further eliminate redundant prime
implicants when solving the PI chart, applying the principle of row dominance: if
a prime implicant \bold{\code{X}} covers the same configurations as another prime
implicant \bold{\code{Y}} and in the same time covers other configurations which
\bold{\code{Y}} does not cover, then \bold{\code{Y}} is redundant and eliminated.

When solving the PI chart, the algorithm finds the minimal number of prime implicants
needed (\bold{\code{k}}) to cover all configurations, then finds all possible pairs
of \bold{\code{k}} prime implicants which do cover those configurations. The argument
\bold{\code{all.sol}} presents all possible combinations of \bold{\code{n}} prime
implicants which solves the PI chart, where \bold{\code{n >= k}}.

\bold{\code{all.sol}} deactivates the argument \bold{\code{row.dom}}, thus inflating 
the number of possible solutions. Depending on the complexity of the PI chart, sometimes
it is not even possible to get all possible solutions.

The argument \bold{\code{omit}} is used to exclude truth table rows from the minimization
process, from the positive configurations and/or from the remainders. It can be specified
as a vector of truth table line numbers, or as a matrix of causal combinations.

The argument \bold{\code{dir.exp}} is used to specify directional expectations, as
described by Ragin (2003). They can be specified as a single string, with values
separated by commas. For multi-value directional expectations, they are specified
together, separated by semicolons. The total length of the directional expectations
must match the number of causal conditions specified in the analysis, using a dash
\bold{\code{"-"}} if there are no particular expectations for a specific causal
condition.

Activating the \bold{\code{details}} argument has the effect of printing parameters
of fit for each prime implicant and each overall solution, the essential prime
implicants being listed in the top part of the table. It also prints the truth table,
in case the argument \bold{\code{data}} has been provided as a data frame instead
of a truth table object.

When argument \bold{\code{show.cases}} is set to \bold{\code{TRUE}}, the case names
will be printed at their corresponding row in the truth table, and also at their
corresponding prime implicants in the table containing the parameters of fit. Cases
separated by commas belong to the same truth table row, while groups separated by
semicolons belong to different truth table rows.

The argument \bold{\code{inf.test}} combines the inclusion score with a statistical
inference test, in order to assign values in the output column from the truth table
(assuming the argument \bold{\code{data}} is not already a truth table object). 
For the moment, it is only the binomial test, which needs crisp data (it doesn't work
with fuzzy sets). For a given (specified) critical significance level, the output for
a truth table row will be coded as:


\tabular{rl}{
\bold{\code{"1"}} \tab if the true inclusion score is significanly higher than
                       \bold{\code{ic1}},\cr \cr
\bold{\code{"C"}} \tab contradiction, if the true inclusion score is not significantly
                       higher than \bold{\code{ic1}}\cr
                  \tab but significantly higher than \bold{\code{ic0}},\cr \cr
\bold{\code{"0"}} \tab if the true inclusion score is not significantly higher than
                       \bold{\code{ic0}}.\cr
}

It should be noted that statistical tests perform well only when the number of cases is
large, otherwise they are usually not significant. For a low number of cases, depending
on the inclusion cutoff value(s), it will be harder to code a value of \bold{\code{"1"}}
in the output, and also harder to obtain contradictions if the true inclusion is not
signficantly higher than \bold{\code{ic0}}.

The argument \bold{\code{use.letters}} controls using the original names of the causal
conditions, or replace them by single letters in alphabetical order. If the
causal conditions are already named with single letters, the original letters
will be used.

Starting with version 2.6, the package employes a different search algorithm based on
consistency cubes, considering all possible combinations of causal conditions and all
possible combinations of their respective levels. The structure of the input dataset
(number of causal conditions, number of levels, number of unique rows in the truth table)
has a direct implication on the search time, as all of those characteristics become
entry parameters when calculating all possible combinations.
}

\value{
An object of class \code{"qca"} when using a single outcomes, or class \code{"mqca"}
when using multiple outcomes. These objects are lists having the following components:

\tabular{rl}{
  \bold{tt} \tab {The truth table object.}\cr
  \bold{excluded} \tab {The line number(s) of the negative configuration(s).}\cr
  \bold{initials} \tab {The initial positive configuration(s).}\cr
  \bold{PIs} \tab {The prime implicant(s).}\cr
  \bold{PIchart} \tab {A list containing the PI chart(s).}\cr
  \bold{solution} \tab {A list of solution(s).}\cr
  \bold{essential} \tab {A list of essential PI(s).}\cr
  \bold{pims} \tab {A list of PI membership scores.}\cr
  \bold{SA} \tab {A list of simplifying assumptions.}\cr
  \bold{i.sol} \tab {A list of components specific to intermediate solution(s), 
                     each having a prime implicants}\cr
               \tab {chart, prime implicant membership scores, (non-simplifying)
                     easy counterfactuals and}\cr
               \tab {difficult counterfactuals.}\cr
}

}

\author{
Adrian Dusa
}

\references{
Cebotari, V.; Vink, M.P. (2013) \dQuote{A Configurational Analysis of Ethnic
Protest in Europe}. \emph{International Journal of Comparative Sociology}
vol.54, no.4, pp.298-324.

Cebotari, V.; Vink, M.P. (2015) \dQuote{Replication Data for: A configurational
analysis of ethnic protest in Europe}, DOI:
\url{http://dx.doi.org/10.7910/DVN/PT2IB9}, Harvard Dataverse, V2

Cronqvist, L.; Berg-Schlosser, D. (2009) \dQuote{Multi-Value QCA (mvQCA)}, in
Rihoux, B.; Ragin, C. (eds.) \emph{Configurational Comparative Methods. Qualitative
Comparative Analysis (QCA) and Related Techniques}, SAGE.

Dusa, A.; Thiem, A. (2015) \dQuote{Enhancing the Minimization of Boolean and
Multivalue Output Functions With eQMC} \emph{Journal of Mathematical Sociology}
vol.39, no.2, pp.92-108.

Ragin, C. (2003) \emph{Recent Advances in Fuzzy-Set Methods and Their Application to Policy Questions}.
WP 2003-9, COMPASSS.\cr
URL: \url{http://www.compasss.org/wpseries/Ragin2003a.pdf}.

Ragin, C. (2009) \dQuote{Qualitative Comparative Analysis Using Fuzzy-Sets (fsQCA)},
in Rihoux, B.; Ragin, C. (eds.) \emph{Configurational Comparative Methods.
Qualitative Comparative Analysis (QCA) and Related Techniques}, SAGE.

Ragin, C.C.; Strand, S.I. (2008) \dQuote{Using Qualitative Comparative 
Analysis to Study Causal Order: Comment on Caren and Panofsky (2005).} 
\emph{Sociological Methods & Research} vol.36, no.4, pp.431-441.

Rihoux, B.; De Meur, G. (2009) \dQuote{Crisp Sets Qualitative Comparative Analysis
(mvQCA)}, in Rihoux, B.; Ragin, C. (eds.) \emph{Configurational Comparative Methods.
Qualitative Comparative Analysis (QCA) and Related Techniques}, SAGE.

}

\seealso{\code{\link{truthTable}}, \code{\link{factorize}}}

\examples{

# -----
# Lipset binary crisp data
data(LC)

# the associated truth table
ttLC <- truthTable(LC, "SURV", sort.by = "incl, n")
ttLC

# conservative solution (Rihoux & De Meur 2009, p.57)
cLC <- eqmcc(ttLC)
cLC

# view the Venn diagram for the associated truth table
library(venn)
venn(cLC)

# add details and case names
eqmcc(ttLC, details = TRUE, show.cases = TRUE)

# negating the outcome
ttLCn <- truthTable(LC, "~SURV", sort.by = "incl, n")
eqmcc(ttLCn)

# using a tilde instead of upper/lower case names
eqmcc(ttLCn, use.tilde = TRUE)

# parsimonious solution, positive output
pLC <- eqmcc(ttLC, include = "?", details = TRUE, show.cases = TRUE)
pLC

# the associated simplifying assumptions
pLC$SA

# parsimonious solution, negative output
pLCn <- eqmcc(ttLCn, include = "?", details = TRUE, show.cases = TRUE)
pLCn



# -----
# Lipset multi-value crisp data (Cronqvist & Berg-Schlosser 2009, p.80)
data(LM)

# truth table 
ttLM <- truthTable(LM, "SURV", conditions = "DEV, URB, LIT, IND",
        sort.by = "incl", show.cases = TRUE)

# conservative solution, positive output
eqmcc(ttLM, details = TRUE, show.cases = TRUE)

# parsimonious solution, positive output
eqmcc(ttLM, include = "?", details = TRUE, show.cases = TRUE)

# negate the outcome
ttLMn <- truthTable(LM, "~SURV", conditions = "DEV, URB, LIT, IND",
         sort.by = "incl", show.cases = TRUE)

# conservative solution, negative output
eqmcc(ttLMn, details = TRUE, show.cases = TRUE)

# parsimonious solution, positive output
eqmcc(ttLMn, include = "?", details = TRUE, show.cases = TRUE)



# -----
# Lipset fuzzy sets data (Ragin 2009, p.112)
data(LF)

# truth table using a very low inclusion cutoff
ttLF <- truthTable(LF, "SURV", incl.cut = 0.7,
        show.cases = TRUE, sort.by="incl")

# conservative solution
eqmcc(ttLF, details = TRUE, show.cases = TRUE)

# parsimonious solution
eqmcc(ttLF, include = "?", details = TRUE, show.cases = TRUE)

# intermediate solution using directional expectations
iLF <- eqmcc(ttLF, include = "?", details = TRUE, show.cases = TRUE,
             dir.exp = "1,1,1,1,1")


# -----
# Cebotari & Vink (2013, 2015)
data(CVF) 

ttCVF <- truthTable(CVF, outcome = "PROTEST", incl.cut = 0.8,
	                show.cases = TRUE, sort.by = "incl, n")

pCVF <- eqmcc(ttCVF, include = "?", details = TRUE, show.cases = TRUE)
pCVF

# inspect the PI chart
pCVF$PIchart

# DEMOC*ETHFRACT*poldis is dominated by DEMOC*ETHFRACT*GEOCON
# using row dominance to solve the PI chart
pCVFrd <- eqmcc(ttCVF, include = "?", row.dom = TRUE,
                details = TRUE, show.cases = TRUE)

# plot the prime implicants on the outcome
pims <- pCVFrd$pims

par(mfrow = c(2, 2))
for(i in 1:4) {
    XYplot(pims[, i], CVF$PROTEST, cex.axis = 0.6)
}


# -----
# temporal QCA (Ragin & Strand 2008)
data(RS)
eqmcc(RS, "REC", details = TRUE, show.cases = TRUE)

}

\keyword{functions}


