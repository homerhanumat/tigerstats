#' @title Diagnostic Plots for GC Linear Regression

#' @description Used by generic plot function
#' 
#' @rdname plot.GClm
#' @method plot GClm
#' @usage 
#' \S3method{plot}{GClm}(x,...)
#' @param x An object of class GClm
#' @param \ldots ignored
#' @return two diagmostic plots
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' SpeedModel <- lmGC(fastest~GPA,data=m111survey)
#' plot(SpeedModel)
plot.GClm <-function(x,...)  {
  
    GClm <- x
    mod <- GClm$mod
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
    
  }#end plot.GClm