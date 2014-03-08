\name{RandomExp}
\alias{RandomExp}
\title{Randomized Experimental Designs}
\usage{
RandomExp(data,sizes=NULL,groups=NULL,block=NULL,seed=NULL)
}
\arguments{
  \item{data}{A data frame containing the subjects to be
  randomized}

  \item{sizes}{a numeric vector indicating the sizes of the
  treatment groups. Vector must sum to the number of
  subjects.  If not provided, subjects will be randomized
  into two groups of size as nearly equal as possible.}

  \item{groups}{a character vector giving the names of the
  groups.  Names correspond to sizes specified in previous
  \code{sizes}.  Length of \code{groups} must equal length
  of \code{sizes}.}

  \item{block}{Variable(s) in the data frame with respect
  to which blocking is performed.  In order to block with
  respect to more than one variable at once, enter as
  character vector, e.g.: c("Var1","Var2").}

  \item{seed}{randomization seed, for reproducibility of
  results.}
}
\value{
A data frame: the input frame \code{data} augmented with a
variable \code{treat.grp} indicating the assignment of
subjects to groups.
}
\description{
Randomizes subjects into treatment groups according to
specified criteria.
}
\examples{
data(SmallExp) #small hypothetical list of subjects

#completely randomized design
RandomExp(SmallExp)

#Block with reppect to sex:
RandomExp(SmallExp,sizes=c(8,8),groups=letters[1:2],block="sex")

#Block for both sex and athletic status:
RandomExp(SmallExp,sizes=c(8,8),groups=letters[1:2],block=c("sex","athlete"))
}
\author{
Homer White \email{hwhite0@georgetowncollege.edu}
}

