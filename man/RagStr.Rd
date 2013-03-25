\name{RagStr}

\docType{data}

\alias{RagStr}

\title{Graduate Student Union Recognition at Research Universities}

\description{
The data frame \bold{RagStr} has 17 rows and 6 columns.

This data is originally from the hypothetical study by Caren and Panofsky (2005), which
analyzes the determinants of unionization attempts by graduate student workers at research
universities using tQCA. The data has been reanalyzed in the study by Ragin and Strand (2008).
}

\usage{data(RagStr)}

\format{
The data contains the following sets:

\tabular{rrl}{
 integer \tab \bold{P}   \tab Condition: public university ("1" yes, "0" no) \cr
 integer \tab \bold{E}   \tab Condition: support of elite allies ("1" yes, "0" no) \cr
 integer \tab \bold{A}   \tab Condition: national union affiliation ("1" yes, "0" no) \cr
 integer \tab \bold{S}   \tab Condition: a strike or strike threat ("1" yes, "0" no) \cr
 factor  \tab \bold{EBA} \tab Condition: \bold{E} present before \bold{A} ("1" yes, "0" no, "dc" don't care) \cr
 integer \tab \bold{REC} \tab Outcome  : union recognition ("1" yes, "0" no)
}
}

\source{
N. Caren and A. Panofsky. TQCA: A Technique for Adding Temporality to Qualitative Comparative Analysis.
\emph{Sociological Methods & Research}, 34(2):147-172, 2005.

C. C. Ragin and S. I. Strand. Using Qualitative Comparative Analysis to Study Causal Order: Comment on Caren and Panofsky (2005).
\emph{Sociological Methods & Research}, 36(4):431-441, 2008.
}

\keyword{datasets}