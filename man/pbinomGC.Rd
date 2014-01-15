\name{pbinomGC}
\alias{pbinomGC}
\title{Graphical Calculator for Binomial Curve Probabilities}
\usage{
  pbinomGC(bound,region="below",size=100,prob=0.5,graph=FALSE)
}
\arguments{
  \item{bound}{A numerical vector of length 1 or 2, range
  of shaded rectangles}

  \item{region}{A character string.  Default is "below".
  Possible values are "between" (when boundary consists of
  two numbers), "below", "above", and "outside" (again when
  boundary consists of two numbers)}

  \item{size}{Number of trials}

  \item{prob}{Probability of success}

  \item{graph}{produce graph?}
}
\value{
  Numerical value of probability.
}
\description{
  Shades desired areas under rectangles of probability
  histogram for binomial, returns numerical value of the
  area.
}
\examples{
pbinomGC(c(45,55),region="between",size=100,prob=0.50)
pbinomGC(6,region="above",size=10,prob=0.7)
}
\author{
  Homer White \email{hwhite0@georgetowncollege.edu}
}

