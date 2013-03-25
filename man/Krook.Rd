\name{Krook}

\docType{data}

\alias{Krook}

\title{Women's Representation in Western-Democratic Parliaments}

\description{
The data frame \bold{Krook} has 22 rows and 6 columns.

This data is from the study by Krook (2010), which analyzes the determinants of
high women's representation in Western-democratic parliaments using csQCA.
}

\usage{data(Krook)}

\format{
The data contains the following sets:

\tabular{rrl}{
 integer \tab \bold{ES}  \tab Condition: PR electoral system ("1" yes, "0" no) \cr
 integer \tab \bold{QU}  \tab Condition: quota for women ("1" yes, "0" no) \cr
 integer \tab \bold{WS}  \tab Condition: social-democratic welfare system ("1" yes, "0" no) \cr
 integer \tab \bold{WM}  \tab Condition: autonomous women's movement ("1" yes, "0" no) \cr
 integer \tab \bold{LP}  \tab Condition: seats held by left-libertarian parties ("1" at least 7 percent, "0" less than 7 percent) \cr
 integer \tab \bold{WNP} \tab Outcome  : women in single/lower house of parliament ("1" >= 30 percent, "0" < 30 percent)
}
}

\note{
The row names are the official International Organization for Standardization (ISO) country code elements as 
specified in ISO 3166-1-alpha-2.
}

\source{
M. L. Krook. Women's Representation in Parliament: A Qualitative Comparative Analysis.
\emph{Political Studies}, 58(5):886-908, 2010.
}


\keyword{datasets}