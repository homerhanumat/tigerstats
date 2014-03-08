\name{MeanSampler}
\alias{MeanSampler}
\title{Repeated Sampling for a Mean (Slow)}
\usage{
MeanSampler(form,data,max.sample.size=30,show.sample=FALSE)
}
\arguments{
  \item{form}{an object of class formula, of the form ~x,
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
An app to explore the sampling distribution of the sample
mean.  The user takes one sample at a time from a given
population.  Output to the console describes relevant
features of the sample, and graphical output updates the
empirical distribution of the sample mean.
}
\note{
Uses \code{manipulate}.
}
\examples{
\dontrun{
data(imagpop)
if (require(manipulate)) MeanSampler(~income,data=imagpop)
}
}
\author{
Homer White \email{hwhite0@georgetowncollege.edu}
}

