#' @title Print Function for GC Linear Regression

#' @description Utility diagnostic plot
#' 
#' @rdname plot.GClm
#' @method plot GClm
#' @usage 
#' \S3method{plot}{GClm}(x,y,...)
#' @param x An object of class GCttest.
#' @param y ignored
#' @param \ldots ignored
#' @usage plot.GClm(x,y,...)
#' @param GClm an object of class GClm
#' @return graphical output
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
plot.GClm <-function(x,y,...)  {
  GClm <- x
  p1 <- densityplot(~GClm$residuals,xlab="residuals",main="Residuals")
  p2 <- xyplot(GClm$residuals~GClm$fitted.values,xlab="predicted y values",
               ylab="residuals",main="Residuals vs. Fits",
               panel=function(...){
                 panel.xyplot(...)
                 panel.abline(h=0)
               })   
print(p1,split=c(1,1,1,2), more=TRUE)
print(p2,split=c(1,2,1,2))         
  }#end plot.GClm