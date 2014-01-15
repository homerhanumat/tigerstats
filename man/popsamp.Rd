\name{popsamp}
\alias{popsamp}
\title{Sampling From a Population}
\usage{
  popsamp(n,pop,...)
}
\arguments{
  \item{n}{number of items to sample}

  \item{pop}{data frame, from which to sample n rows}

  \item{\ldots}{other arguments passed to function}
}
\value{
  The sample, as a data frame.
}
\description{
  Instructional function, and possibly a utility function
  for some apps.
}
\examples{
data(imagpop)
popsamp(10,imagpop)  #Simple random sampling (no replacement)
popsamp(10,imagpop,replace=TRUE)  #Random sampling with replacement
}
\author{
  Homer White \email{hwhite0@georgetowncollege.edu}
}

