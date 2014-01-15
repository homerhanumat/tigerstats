\name{ptGC}
\alias{ptGC}
\title{Graphical Calculator for t-Curve Probabilities}
\usage{
  ptGC(bound,region="between",df=1,graph=FALSE)
}
\arguments{
  \item{bound}{A numerical vector of length 1 or 2,
  indicating boundary(ies) of shaded region on horizontal
  axis}

  \item{region}{A character string.  Possible values are
  "between" (when boundary consists of two numbers),
  "below", "above", and "outside" (again when boundary
  consists of two numbers)}

  \item{df}{degrees of freedom of the distribution}

  \item{graph}{produce graph?}
}
\value{
  Numerical value of area under curve over region.  Also
  plots the t-curve with the shaded area.
}
\description{
  Shades desired areas under a specified t-curve, returns
  numerical value of the area.
}
\examples{
ptGC(c(-1,1),region="outside",df=1)
ptGC(-1,region="below",df=5)
}
\author{
  Homer White \email{hwhite0@georgetowncollege.edu}
}

