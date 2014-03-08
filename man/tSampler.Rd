\name{tSampler}
\alias{tSampler}
\title{Illustrating the t-statistic}
\usage{
tSampler(form,data,max.sample.size=30,show.sample=FALSE)
}
\arguments{
  \item{form}{An object of class formula, of the form ~x,
  where x is a numeric variable from the data frame
  supplied by:}

  \item{data}{A dataframe, representing the imaginary
  population.}

  \item{max.sample.size}{Maximum sample size on the
  slider.}

  \item{show.sample}{If TRUE, the complete sample will be
  output to the console, in addition to the summary
  information.}
}
\value{
Graphical and numerical output.
}
\description{
An app to explore the distribution of the t-statistic.  The
user takes one sample at a time from a given population.
Graphical output updates the empirical distribution of the
sample mean.
}
\note{
Uses \code{manipulate}.
}
\examples{
\dontrun{
data(imagpop)
if (require(manipulate)) tSampler(~income,data=imagpop)
}
}
\author{
Homer White \email{hwhite0@georgetowncollege.edu}
}

