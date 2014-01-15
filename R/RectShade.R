#' @title Shade Rectangles for Discrete Distributions

#' @description Utility function for pbinomGC ...
#' 
#' @rdname RectShade
#' @usage RectShade(low,high,func,...)
#' @param low lower bound
#' @param high upper bound
#' @param func probability mass function
#' @param \ldots other arguments passed (to modify func)
#' @return graphical side effect only
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
RectShade <- function(low,high,func,...) { #Utility
  range <- seq(low,high,by=1)
  for (n in range) {
    rect(n-0.5,0,n+0.5,func(n),col="lightblue")
  }
}