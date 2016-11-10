\name{fuzzyand, fuzzyor}

\alias{fuzzyand}
\alias{fuzzyor}

\title{Logical operations}

\description{
These functions perform logical "and" and "or", either for binary crisp or fuzzy
set membership scores.
}

\usage{
fuzzyand(...)

fuzzyor(...)
}

\arguments{
  \item{...}{Two or more numerical (calibrated) membership scores, or a matrix / data frame
           of calibrated columns.}
}


\value{A numerical vector of resulting values}

\author{
Adrian Dusa
}


\examples{

# -----
# Cebotari & Vink (2013, 2015)
data(CVF)

# DEMOC*GEOCON*NATPRIDE
fuzzyand(CVF$DEMOC, CVF$GEOCON, CVF$NATPRIDE)

# same thing with
fuzzyand(CVF[, c(1,3,5)])

# DEMOC*geocon*NATPRIDE
fa <- fuzzyand(CVF$DEMOC, 1 - CVF$GEOCON, CVF$NATPRIDE)
fa

attr(fa, "name")



# ETHFRACT + POLDIS
fuzzyor(CVF$ETHFRACT, CVF$POLDIS)

# same thing with
fuzzyor(CVF[, c(2,4)])

# ETHFRACT + poldis
fo <- fuzzyor(CVF$ETHFRACT, 1 - CVF$POLDIS)
fo

attr(fo, "name")

}

\keyword{functions}
