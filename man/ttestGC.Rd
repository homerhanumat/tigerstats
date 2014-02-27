\name{ttestGC}
\alias{ttestGC}
\title{t-Procedures}
\usage{
  ttestGC(x=NULL,mean=numeric(),sd=numeric(),n=numeric(),
    mu=NULL,data=NULL,alternative="two.sided",var.equal=FALSE,
    conf.level=0.95,graph=FALSE,first=NULL,verbose=TRUE)
}
\arguments{
  \item{x}{If not NULL, then must be a formula.  If a
  formula, then data must be a dataframe. For one sample
  t-procedures, x is of the form ~var.  For two-sample
  procedures, x is of the form resp~exp, where exp is
  factor with two values.  If x is of form ~var1-var2, then
  matched pairs procedures are performed .}

  \item{mean}{When not NULL, contains sample mean(s).
  Length 1 for one sample t-procedures, Length 2 for
  two-sample procedures.}

  \item{sd}{When not NULL, contains sample standard
  deviation(s).}

  \item{n}{When not NULL, contains sample size(s).}

  \item{mu}{Contains the null value for the parameter of
  interest.  If not set, no test is performed.}

  \item{data}{A data frame containing variables in formula
  x.  Required when x is assigned..}

  \item{alternative}{"two.sided" requests computation of a
  two-sided P-value; other possible values are "less" and
  "greater".}

  \item{var.equal}{When FALSE, use Welch's approximation to
  the degrees of freedom.}

  \item{conf.level}{Number between 0 and 1 indicating the
  confidence-level of the interval supplied.}

  \item{graph}{If TRUE, plot graph of P-value.}

  \item{first}{If assigned, gives the value of the
  explanatory variable that is to count as the first
  sample.}

  \item{verbose}{Indicate how much output goes to console}
}
\value{
  A list of class "GCttest" Components of the list that may
  be usefully queried include: "statistic", "p.value", and
  "interval".
}
\description{
  t-tests and confidence intervals for one and two samples.
}
\examples{
# One-sample t:
ttestGC(~fastest,data=m111survey,mu=100)

#Two-sample t, with graph:
ttestGC(fastest~sex,data=m111survey)

# Matched pairs:
ttestGC(~ideal_ht-height,data=m111survey)

#Summary data, one sample:
ttestGC(mean=55,sd=4,n=16,mu=52.5,alternative="greater")

#Summary data, two samples:
ttestGC(mean=c(50,55),sd=c(3,4),n=c(25,40),mu=0)
}
\author{
  Homer White \email{hwhite0@georgetowncollege.edu} for
  matched pairs.
}

