\name{NEWS}
\title{NEWS file for the tigerstats package}

\section{Changes in version 0.1.4}{
\itemize{

\item Addition of three new Shiny apps: versions of the manipulate apps \code{CIMean} and \code{Type12Errors}, and a third app \code{CoinFlip} that provides an early introduction to binomial random variables in an inferential context.  To run the Shiny apps locally from an R session, install package \code{shiny} from CRAN and then at the console type commands like \code{shiny::runApp(system.file('CIMean', package='tigerstats'))}

}
}

\section{Changes in version 0.1.3}{
\itemize{

\item Several of the instructional applets (\code{FindRegLine}, \code{ShallowReg}, and \code{SlowGoodness}) now appear in alternative form as Shiny apps.  Source code for these apps is contained in the \code{inst} directory of the package.  To run the apps locally from an R session, install package \code{shiny} from CRAN and then at the console type something like \code{shiny::runApp(system.file('SlowGoodness', package='tigerstats'))}

}
}