\name{Variability}
\alias{Variability}
\title{Variability}
\usage{
  Variability()
}
\value{
  Graphical and numerical output
}
\description{
  An app to investigate how the variance and sample size
  affects the shape of a histogram and violin plot
  generated from normal data. Summary data (minimum,
  median, mean, maximum, and quartiles) are displayed in
  the output for each random sample drawn.
}
\note{
  Uses \code{histogram} and \code{bwplot} from the
  \code{lattice} package.
}
\examples{
\dontrun{
if (require(manipulate)) Variability()
}
}
\author{
  Rebekah Robinson
  \email{rebekah_robinson@georgetowncollege.edu}
}

