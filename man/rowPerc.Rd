\name{rowPerc}
\alias{rowPerc}
\title{Row Percents}
\usage{
rowPerc(tab)
}
\arguments{
  \item{tab}{A table, e.g., the result of
  \code{xtabs(~var1+var2,data=DataFrame)}.}
}
\value{
An object of class \code{table}, giving row percentages for
the input table.
}
\description{
Computes row percentages for a given twoway table.
}
\examples{
data(ledgejump)
MyTable <- xtabs(~weather+crowd.behavior,data=ledgejump)
rowPerc(MyTable)
}
\author{
Homer White \email{hwhite0@georgetowncollege.edu}
}

