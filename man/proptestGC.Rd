\name{proptestGC}
\alias{proptestGC}
\title{Proportions Procedures}
\usage{
proptestGC(x,n=numeric(),p=NULL,data,alternative="two.sided",
                         success="yes",first=NULL,conf.level=0.95,correct=TRUE,graph=FALSE,verbose=TRUE)
}
\arguments{
  \item{x}{Either a formula or a numeric vector.  If
  formula, it must be of the form ~x indicating the single
  variable under study, or of the form ~x+y, in which case
  x is the explanatory grouping variable (categorical with
  two values) and y is the response categorical variable
  with two values. When summary data are provided, x is a
  numeric vector of success counts.}

  \item{n}{When not empty, this is a numeric vector giving
  the size of each sample.}

  \item{p}{Specifies Null Hypothesis value for population
  proportion.  If not set, no test is performed.}

  \item{data}{Data frame that supplies the variables x and
  y.}

  \item{alternative}{"two.sided" requests computation of a
  two-sided P-value; other possible values are "less" and
  "greater".}

  \item{success}{When x is a formula, this argument
  indicates which value of variable x (in case of ~x) or y
  (in case of ~x+y) is being counted as a success.  When
  working with formula-data input the value of this
  parameter MUST be set, even when the variable has only
  two values.}

  \item{first}{When performing 2-sample procedures, this
  argument specifies which value of the explanatory
  variable constitutes the first group.}

  \item{conf.level}{Number between 0 and 1 indicating the
  confidence-level of the interval supplied.}

  \item{correct}{Applies continuity correction for
  one-proportion procedures.  It is ignored when when
  2-proportions are performed.}

  \item{graph}{If TRUE, plot graph of P-value.}

  \item{verbose}{Indicates how much output goes to the
  console}
}
\value{
A list, either of class "gcp1test" (one-proportion) or
"gcp2test" (two proportions). Components of this list that
may be usefully queried include: "statistic", "p.value",
and "interval".
}
\description{
Employs the normal approximation to perform test for one or
two proportions.
}
\examples{
data(m111survey)
#2-proportions, formula-data input, 95\%-confidence interval only:
proptestGC(~sex+seat,data=m111survey,success="2_middle")

#For other confidence levels, use argument conf.level.  For 90\%-interval for one proportion p:
proptestGC(~sex,data=m111survey,success="male",conf.level=0.90)

#one proportion, formula-data input, confidence interval and two-sided test with H_0:  p = 0.33:
proptestGC(~seat,data=m111survey,success="1_front",p=0.33)

#Summary data:
#In first sample, 23 successes out of 100 trials.  In second sample, 33 out of 110.
proptestGC(x=c(23,33),n=c(100,110))

#Summary data:
#In one sample, 40 successes in 100 trials.  Testing whether p = 0.45.
proptestGC(40,100,p=0.45,correct=TRUE)

#Want less output?  Set argument verbose to FALSE:
proptestGC(~sex+seat,data=m111survey,success="2_middle",p=0.33,verbose=FALSE)
}
\author{
Homer White \email{hwhite0@georgetowncollege.edu}
}

