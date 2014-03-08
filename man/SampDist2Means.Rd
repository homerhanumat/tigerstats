\name{SampDist2Means}
\alias{SampDist2Means}
\title{Distribution of the Difference of Sample Means}
\usage{
SampDist2Means(pop,max.samp.sizes=50,sim.reps=1000)
}
\arguments{
  \item{pop}{A data frame representing the population from
  which samples are taken.}

  \item{max.samp.sizes}{Largest sample sizes shown on the
  sliders.}

  \item{sim.reps}{Number of simulation repetitions to
  construct empirical distribution of difference of sample
  means.}
}
\value{
Graphical and numerical output.
}
\description{
An app to explore the Central Limit Theorem in the context
of the difference of sample means.
}
\note{
Uses \code{manipulate} in RStudio.  Also requires package
\code{lattice}.
}
\examples{
\dontrun{
data(imagpop)
if (require(manipulate)) SampDist2Means(imagpop)
}
}
\author{
Homer White \email{hwhite0@georgetowncollege.edu}
}

