\name{colPerc}
\alias{colPerc}
\title{Column Percents}
\usage{
colPerc(tab)
}
\arguments{
  \item{tab}{A two way table, e.g., the result of
  \code{xtabs(~var1+var2,data=DataFrame)}.}
}
\value{
An object of class \code{table}, giving column percentages
for the input table.
}
\description{
Computes column percentages for a given twoway table.
}
\examples{
MyTable <- xtabs(~weather+crowd.behavior,data=ledgejump)
colPerc(MyTable)
}
\author{
Homer White \email{hwhite0@georgetowncollege.edu}
}

