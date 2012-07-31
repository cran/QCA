\name{HarKem}

\docType{data}

\alias{HarKem}

\title{Party Bans Sub-Saharan Africa}

\description{
The data frame \bold{HarKem} has 48 rows and 7 columns.

This data is from the study by Hartmann and Kemmerzell (2010), which analyzes the determinants of
the introduction of party ban provisions and their actual implementation in sub-Saharan Africa using mvQCA.
}

\usage{data(HarKem)}

\format{
The data contains the following sets:

\tabular{rl}{
\bold{C}   \tab Condition: colonial tradition ("2" British, "1" French, "0" other) \cr
\bold{F}   \tab Condition: former regime type competition ("2" no, "1" limited, "0" multi-party) \cr
\bold{T}   \tab Condition: mode of transition ("2" managed, "1" pacted, "0" democracy before 1990) \cr
\bold{R}   \tab Condition: regime type ("2" stable authoritarian, "1" liberalizing/blocked, "0" democratic/democratizing) \cr
\bold{V}   \tab Condition: ethnic violence ("1" yes, "0" no) \cr
\bold{PB}  \tab Outcome  : party ban provisions introduced ("1" yes, "0" no) \cr
\bold{PBI} \tab Outcome  : party bans implemented ("1" yes, "0" no)
}
}

\note{
The row names are the official International Organization for Standardization (ISO) country code elements as 
specified in ISO 3166-1-alpha-2.
}

\source{
C. Hartmann and J. Kemmerzell. Understanding Variations in Party Bans in Africa.
\emph{Democratization}, 17(4):642-665, 2010.
}

\keyword{datasets}