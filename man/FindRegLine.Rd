\name{FindRegLine}
\alias{FindRegLine}
\title{Find the Regression Line}
\usage{
  FindRegLine
}
\value{
  Graphical and numerical output.
}
\description{
  The regression minimizes the residual sum of squares
  (RSS).  In this game, the player chooses slope and
  y-intercept of a line so as to approximate the regression
  line.  The moveable line is set initially as a horizontal
  line with height equal to the mean of the y-coordinates
  of the scatterplot, so initially the residual sum of
  squares equals the total sum of squares (TSS). The
  player's score is the sum of the number of turns taken
  and the difference between the current RSS and the
  regression line's RSS (as a percentage of TSS-RSS for
  regression line).  The aim is to lower one's score.
}
\note{
  Requires package \code{manipulate}, available only in
  RStudio.
}
\examples{
\dontrun{
if (require(manipulate)) FindRegLine()
}
}
\author{
  Homer White \email{hwhite0@georgetowncollege.edu}
}

