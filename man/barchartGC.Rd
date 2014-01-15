\name{barchartGC}
\alias{barchartGC}
\title{Easy barcharts from raw and tabular data}
\usage{
  barchartGC(x,data=NULL,type="frequency",main=NULL)
}
\arguments{
  \item{x}{Either a formula or a table.  If formula, it
  must be of the form ~var or ~var1+var2.}

  \item{data}{Usually a data frame that supplies the
  variables in \code{x}.}

  \item{type}{Possible values are "frequency" and
  "percent".}

  \item{main}{An optional title}
}
\value{
  A trellis object describing the barchart.
}
\description{
  Wrapper for \code{\link[lattice]{barchart}} in package
  \code{lattice}.  Creates a barchart from raw data, using
  formula-data syntax similar to that of
  \code{\link{xtabs}}.  There are very few options.
}
\examples{
data(m111survey)
barchartGC(~sex,data=m111survey)

#percentage barchart:
barchartGC(~sex+seat,data=m111survey,type="percent")

#From tabulated data:
sexseat <- xtabs(~sex+seat,data=m111survey)
barchartGC(sexseat,type="percent",main="Sex and Seating Preference")
}
\author{
  Homer White \email{hwhite0@georgetowncollege.edu}
}

