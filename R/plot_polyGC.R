#' @title Diagnostic Plots for GC Polynomial Regression

#' @description Used by generic plot function
#' 
#' @rdname plot.polyGC
#' @method plot polyGC
#' @usage 
#' \S3method{plot}{polyGC}(x,...)
#' @param x An object of class polyGC
#' @param \ldots ignored
#' @return two diagmostic plots
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' mpgModel <- polyfitGC(mpg~wt,data=mtcars)
#' plot(mpgModel)
plot.polyGC <-function(x,...)  {
  
    polyGC <- x
    mod <- polyGC$mod
    residuals <- mod$residuals
    fitted.values <- mod$fitted.values
  
    p1 <- lattice::densityplot(~residuals,xlab="residuals",main="Residuals")
    p2 <- lattice::xyplot(residuals~fitted.values,xlab="predicted y values",
                 ylab="residuals",main="Residuals vs. Fits",pch=19,
                 panel=function(...){
                   lattice::panel.xyplot(...)
                   lattice::panel.abline(h=0)
                 })   
    print(p1,split=c(1,1,1,2), more=TRUE)
    print(p2,split=c(1,2,1,2))
    
  }#end plot.polyGC