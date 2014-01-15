#' @title Print Function for GC Linear Regression

#' @description Utility print function
#' 
#' @rdname print.GClm
#' @method print GClm
#' @usage 
#' \S3method{print}{GClm}(x,...)
#' @param x an object of class GClm
#' @param \ldots ignored
#' @return graphical output and output to console
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
print.GClm <-function(x,...)  {
         GClm <- x
         cat("\n") 
         cat("\t\t\tSimple Linear Regression\n\n")
         cat("Correlation coefficient r = ",cor(GClm$exp,GClm$resp,use="na.or.complete"),"\n\n")
         cat("Equation of Regression Line:\n\n")
         cat("\t",GClm$respname,"=",GClm$intercept,"+",GClm$slope,"*",GClm$expname,"\n")
         cat("\n")
         cat("Residual Standard Error:\ts   =",GClm$resid.sterr,"\n")
         cat("R^2 (unadjusted):\t\tR^2 =",GClm$r.squared,"\n")
         
         if (GClm$graph) print(xyplot(GClm$resp~GClm$exp,type=c("p","r"),xlab=GClm$respname,pch=19,
                           ylab=GClm$expname))
         if (GClm$diag) {
           p1 <- densityplot(~GClm$residuals,xlab="residuals",main="Residuals")
           p2 <- xyplot(GClm$residuals~GClm$fitted.values,xlab="predicted y values",
                        ylab="residuals",main="Residuals vs. Fits",pch=19,
                        panel=function(...){
                          panel.xyplot(...)
                          panel.abline(h=0)
                        })   
           print(p1,split=c(1,1,1,2), more=TRUE)
           print(p2,split=c(1,2,1,2))  
         }
         
  }#end print.GClm