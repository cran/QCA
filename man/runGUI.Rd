\name{runGUI}

\alias{runGUI}

\title{run the GUI shiny app for the QCA package}

\description{
Runs the graphical user interface app based on the \pkg{shiny} package.
}

\usage{
runGUI(x)
}

\arguments{
  \item{x}{Path to the shiny app.}
}

\details{
This function is nothing but a wrapper to the \bold{\code{runApp()}} function in package
\pkg{shiny}.
It locates the \bold{\code{gui}} directory in the path where the QCA package has been
installed, and runs it.

Since version 2.2, the user interface gained an interactive R console in the webpage. Commands are
parsed and evaluated into a dedicated environment, with efforts to capture errors and
warnings. Preliminary tests are encouraging, but users should be aware this is not a
real console and bugs are likely to appear in this stage of development.
}

\author{
Adrian Dusa
}

\keyword{functions}
