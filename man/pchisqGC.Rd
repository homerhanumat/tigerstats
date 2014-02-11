\name{pchisqGC}
\alias{pchisqGC}
\title{Graphical Calculator for Chi-Square Probabilities}
\usage{
  pchisqGC(bound,region="above",df=NA,xlab="chi-square statistic",graph=FALSE)
}
\arguments{
  \item{bound}{A numerical vector of length 1, indicating
  boundary of shaded region on horizontal axis}

  \item{region}{A character string.  Possible values are
  "below" and "above"}

  \item{df}{Degrees of freedom of the chi-square
  distribution}

  \item{xlab}{Label for the horizontal axis}

  \item{graph}{produce graph?}
}
\value{
  Numerical value of area under curve over region.  Also
  plots the chi-square curve with the shaded area.
}
\description{
  Shades desired areas under a specified chi-square curve,
  returns numerical value of the area.
}
\examples{
pchisqGC(6.8,df=3,region="below")
}
\author{
  Homer White \email{hwhite0@georgetowncollege.edu}
}

