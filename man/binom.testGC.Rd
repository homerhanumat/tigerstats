\name{binom.testGC}
\alias{binom.testGC}
\title{Exact Procedures for a Single Proportion}
\usage{
  binom.testGC(x,n=numeric(),p=NULL,data,alternative="two.sided",
    success="yes",conf.level=0.95,graph=FALSE)
}
\arguments{
  \item{x}{Either a formula or a numeric vector.  If
  formula, it must be of the form ~x indicating the single
  variable under study.  When summary data are provided, x
  is a numeric vector of success counts.}

  \item{n}{When not empty, this is a numeric vector giving
  the size of the sample.}

  \item{p}{Specifies Null Hypothesis value for population
  proportion.  If not set, no test is performed.}

  \item{data}{Data frame that supplies the variable x.}

  \item{alternative}{"two.sided" requests computation of a
  two-sided P-value; other possible values are "less" and
  "greater".}

  \item{success}{When x is a formula, this argument
  indicates which value of variable x is being counted as a
  success. When working with formula-data input the value
  of this parameter MUST be set, even when the variable has
  only two values.}

  \item{conf.level}{Number between 0 and 1 indicating the
  confidence-level of the interval supplied.}

  \item{graph}{If TRUE, plot graph of P-value.  Ignored if
  no test is performed.}
}
\value{
  Output to console.  Future versions may return an object,
  and include a print method.
}
\description{
  Wrapper for binom.test in package \code{stats}.  Employs
  the binomial distribution in inferential procedures for a
  single proportion.
}
\examples{
data(m111survey)
binom.testGC(~sex,data=m111survey,success="female") #confidence interval only

binom.testGC(~sex,data=m111survey,success="female",p=0.5) #test included

#Summary data:
#In one sample, 40 successes in 100 trials.  Testing whether p = 0.45.
binom.testGC(40,100,p=0.45)
}
\author{
  Homer White \email{hwhite0@georgetowncollege.edu}
}

