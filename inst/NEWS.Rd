\name{NEWS}
\title{NEWS file for the tigerstats package}

\section{Changes in Version 1.9} {
\itemize{

\item Addition of qnormGC().

\item Addition of check argument to lmGC().  Diagnostics are no longer done with the diag argument; instead one calls the plot() function.

\item Addition of polyfitGC().

\item Addition of henderson data frame; the seals data frame has been re-named to sealsO2 to avoid name conflict with seals in the ggplot2 package.

}
}

\section{Changes in version 0.1.7}{
\itemize{

\item Addition of package vignettes:  tutorials on all major functions, a summary of descriptive functions and a summary of inferential functions.

\item chisqtestGC() now provides a graph of resampled chi-square statistics when simulate.p.value is set to TRUE.

\item barchartGC() accepts more arguments, and adds a FLat" argument."

}
}

\section{Changes in version 0.1.6}{
\itemize{

\item Addition of Shiny app \code{SamplingMethods} and manipulate app \code{SimpleRandom2}..

\item Instructional function expCounts() computes expected cell counts from a two-way table of observed counts.

\item Major statistical analysis functions that accept formula data input are now more responsive to the
environment in which they are called.  If variables in the formula are not present in the data frame
supplied by the data argument (or if no value is supplied by the user), then they will be searched for in the parent environment of the called function, and then up through the chain. This change afffects binomtestGC(), chisquaretestGC(), proptestGC() and ttestGC().  Instructional functions such as
manipulate apps have not been thus re-written.

}
}

\section{Changes in version 0.1.5}{
\itemize{

\item Addition of baseball datasets:  \code{hofbatting}, \code{hofpitching}, \code{verlander} and \code{cabrera}.
\item Addition of Shiny app \code{CentralLimit}.

}
}

\section{Changes in version 0.1.4}{
\itemize{

\item Addition of four new Shiny apps: versions of the manipulate apps \code{CIMean} and \code{Type12Errors}, and and two new apps: \code{CoinFlip} that provides an early introduction to binomial random variables in an inferential context, and \code{RandomExpBinom} that explores experiments with two treatment groups in a completely randomized design in which the response variable is Bernoulli.  To run the Shiny apps locally from an R session, install package \code{shiny} from CRAN and then at the console type commands like \code{shiny::runApp(system.file('CIMean', package='tigerstats'))}

}
}

\section{Changes in version 0.1.3}{
\itemize{

\item Several of the instructional applets (\code{FindRegLine}, \code{ShallowReg}, and \code{SlowGoodness}) now appear in alternative form as Shiny apps.  Source code for these apps is contained in the \code{inst} directory of the package.  To run the apps locally from an R session, install package \code{shiny} from CRAN and then at the console type something like \code{shiny::runApp(system.file('SlowGoodness', package='tigerstats'))}

}
}