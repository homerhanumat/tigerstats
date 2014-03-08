\name{chisq.testGC}
\alias{chisq.testGC}
\title{Chi-Square Test (GC version)}
\usage{
chisq.testGC(x,data=NULL,p=NULL,graph=FALSE,simulate.p.value=FALSE,B=2000,verbose=TRUE)
}
\arguments{
  \item{x}{Could be a formula.  If so, either ~var (for
  goodness of fit) or ~var1+var2 (for test for
  association). Otherwise either a table, matrix or vector
  of summary data.}

  \item{data}{dataframe supplying variables for formula x.}

  \item{p}{For goodness of fit, a vector of probabilities.
  This will be automatically scaled so as to sum to 1.
  Negative elements result in an error message.}

  \item{graph}{produce relevant graph for P-value
  (chi-square curve or histogram of simulation results).
  Ignored if user requests R's resampling routines (see
  below).}

  \item{simulate.p.value}{If FALSE, use a chi-square
  distribution to estimate the P-value.  Other possible
  values are "random" and "fixed" and TRUE.  Random effects
  are suitable for resampling when the data are a random
  sample from a poulation.  Fixed effects assume that the
  values of the explanatory variable (row variable for
  table, var1 in formula ~var1+var2) remain fixed in
  resampling, and values of response variable are random
  with null distribution estimated from the data.  When set
  to TRUE, we use R's resampling routines.}

  \item{B}{number of resamples to take.}

  \item{verbose}{If TRUE, include lots of information in
  the output.}
}
\value{
No value, just side effects.  Future versions may define an
S3 object, with print method.
}
\description{
Perform chi-square test, either goodness of fit or test for
association.  Enter either formula-data input or a summary
table.  Simulation is optional.
}
\note{
Deprecated in favor of \code{chisqtestGC}.  Will be removed
eventually.
}
\examples{
chisq.testGC(~seat,data=m111survey,p=c(1/3,1/3,1/3))
chisq.testGC(~sex+seat,data=m111survey)

WeBe <- xtabs(~weather+crowd.behavior,data=ledgejump)
chisq.testGC(WeBe,simulate.p.value="fixed",B=2500)
}
\author{
Homer White \email{hwhite0@georgetowncollege.edu}
}

