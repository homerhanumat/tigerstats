\name{NEWS}
\title{NEWS file for the tigerstats package}

\section{Changes in version 0.1.5}{
\itemize{

\item Addition of baseball datasets:  \code{hofbatting}, \code{verlander}.

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