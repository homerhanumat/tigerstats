\name{theme.rpres}
\alias{theme.rpres}
\title{Lattice Theme or R Presentations}
\usage{
  theme.rpres()
}
\value{
  Returns a list to be supplied as the \code{theme} to the
  \code{lattice} function \code{\link{trellis.par.set}()}.
}
\description{
  Modifies the current theme for use with lattice graphics
  in R Presentation dicuments. Increases size of title,
  axis lables and axis numbers, thickens sone lines, etc.
}
\examples{
trellis.par.set(theme=theme.rpres())
}
\seealso{
  \code{\link{trellis.par.set}},
  \code{\link{show.settings}}
}
\keyword{graphics}

