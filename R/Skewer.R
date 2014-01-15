#' @title SkewR

#' @description An app to illustrate the effect of skewness on the shape of a boxplot.
#' 
#' @rdname Skewer
#' @usage Skewer()
#' @return Graphical output.
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @note Requires \code{manipulate}; uses functions from package \code{lattice}
#' @examples
#' \dontrun{
#' if (require(mainpulate)) Skewer()
#' }
Skewer <- function () 
{
  b <- 10
  
  beta.100 <- function(x,alpha,beta) { #density function for population ~ 100*beta
    0.01*dbeta(x/100,shape1=alpha,shape2=beta)
  }
  
  ymax <- max(beta.100(0:100,alpha=1,beta=10))  #set upper limit for histogram graph
  
  manipulate(alpha = slider(0, 1, init = 0, label = "Population Skew:  0 = Symmetric, 1=Fairly Skewy "),
             showpop=checkbox(FALSE,"Show Population Density Curve"),
{
  a <- 10 * (0.1)^alpha
  rand.data <- 100 * rbeta(1000, shape1 = a, shape2 = b)
  x.den <- seq(0,100,by=0.1)
  y.den <- beta.100(x.den,alpha=a,beta=b)
  
  if (showpop)  {
    p1 <- histogram(~rand.data, type = "density", main = "Histogram of Some Random Data", 
                    xlim = c(-5, 105), ylim = c(0, ymax), xlab = "x",
                    breaks=seq(0,100,by=5),
                    panel=function(x,...) {
                      panel.histogram(x, ...)
                      panel.lines(x.den,y.den,col="red")
                    }
    )
  }else {
    p1 <- histogram(~rand.data, type = "density", main = "Histogram of Some Random Data", 
                    xlim = c(-5, 105), ylim = c(0, ymax), xlab = "x",
                    breaks=seq(0,100,by=5),
                    panel=function(x,...) {
                      panel.histogram(x, ...)
                    }
    )
  }
  
  p2 <- bwplot(~rand.data, xlim = c(0, 100), main = "Violin Plot of the Same Data", 
               xlab = "x", panel = function(..., box.ratio) {
                 panel.violin(..., col = "bisque", box.ratio = box.ratio)
                 panel.bwplot(..., box.ratio = 0.1)
               })
  cat("median =", round(median(rand.data),2), "\n")
  cat("mean =", round(mean(rand.data),2), "\n")
  cat("\n")
  print(p1, split = c(1, 1, 2, 1), more = TRUE)
  print(p2, split = c(2, 1, 2, 1))
}#end manip body
  )#end manip
}#end Skewer