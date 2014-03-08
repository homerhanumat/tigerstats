\name{Type12Errors}
\alias{Type12Errors}
\title{Type I and Type II Errors}
\usage{
Type12Errors()
}
\value{
Graphical and numerical output.
}
\description{
An app to explore the concepts of Type I and Type II
errors, and the concept of power.  We take samples from a
population that is imagined to be normal, and perform the
t-procedures for one mean.  The Null Hypotheis is H0:
mu=170.  A slider allows us to vary the true mean mu.
}
\note{
Uses \code{manipulate}.
}
\examples{
\dontrun{
if (require(manipulate)) Type12Errors()
}
}
\author{
Homer White \email{hwhite0@georgetowncollege.edu}
}

