\name{NEWS}
\title{NEWS file for the tigerstats package}
\section{Changes in version 0.1.3}{
\itemize{

\item Several of the instructional applets (\code{FindRegLine}, \code{ShallowReg}, and \code{SlowGoodness}) now appear in alternative form as Shiny apps.  Source code for these apps is contained in the \code{inst} direcotry of the package.  To run the apps locally within R Studio, install package \code{shiny} from CRAN and then at the console type something like \code{shiny::runApp(system.file('SlowGoodness', package='tigerstats'))}

}
}