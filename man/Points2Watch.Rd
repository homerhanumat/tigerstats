\name{Points2Watch}
\alias{Points2Watch}
\title{A study of Influence}
\usage{
  Points2Watch()
}
\value{
  Graphical output.
}
\description{
  An app to explore the idea of influence.  Note how the
  influence of the blue point wanes as the number of points
  in the central cloud increases, and also wanes as the
  correlation of the central cloud increases.
}
\note{
  Requires package \code{manipulate}, available only in
  RStudio. Uses \code{mvrnorm} from package \code{MASS}.
}
\examples{
\dontrun{
if (require(manipulate)) Points2Watch()
}
}
\author{
  Homer White \email{hwhite0@georgetowncollege.edu}
}

