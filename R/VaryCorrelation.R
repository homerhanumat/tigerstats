#' @title Vary Correlation

#' @description An app to illustrate the effectiveness of the correlation coefficient as a measure
#' of the strength of a linear relationship.
#' 
#' @rdname VaryCorrelation
#' @usage VaryCorrelation(n=300)
#' @param n number of randomly generated-points in the scatterplot.  
#' @return Graphical output.
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @note  Uses \code{manipulate} in RStudio, and \code{mvrnorm} from package \code{MASS}.
#' @examples
#' \dontrun{
#' if(require(manipulate)) VaryCorrelation(n=500)
#' }
VaryCorrelation <-
function (n=300)  {
  #n is number of points on scatterplot
  manipulate(
    rho=slider(-1,1,step=0.01,init=0,
               label="Target Correlation"),
    reg=checkbox(FALSE,"Show Regression Line"),
    {varcovar <- cbind(c(1,rho),c(rho,1))
    rpoints <- MASS::mvrnorm(n=n,mu=c(0,0),Sigma=varcovar)
    r <- round(cor(rpoints)[1,2],3)
     x <- rpoints[,1]
     y <- rpoints[,2]
    plot(x,y,cex=0.5,col=rgb(0,0,1,0.5),pch=16,
         main=paste("Correlation =",r))
    if(reg==TRUE) {
      mod <- lm(y~x)
      abline(coef(mod),col="red")}
    }
  )
}
