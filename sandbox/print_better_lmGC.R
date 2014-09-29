#' @title Print Function for GC Linear Regression

#' @description Utility print function
#' @keywords internal
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
print.GClm2 <-function(x,...)  {
         GClm <- x
         degree <- GClm$degree
         coefs <- GClm$coefficients
         exp <- GClm$exp
         resp <- GClm$resp
         respname <- GClm$respname
         expname <- GClm$expname
         cat("\n") 
         if (degree==1) {
           cat("\tLinear Regression\n\n")
            cat("Correlation coefficient r = ",signif(cor(GClm$exp,GClm$resp,use="na.or.complete"),4),"\n\n")
            cat("Equation of Regression Line:\n\n")
            cat("\t",respname,"=",round(coefs[1],4),"+",round(coefs[2],4),"*",
                expname,"\n")
         cat("\n")
         }
         
         if (degree==2) {
           cat("\tQuadratic Regression\n\n")
           cat("Equation of Fitted Parabola:\n\n")
           cat("\t",respname,"=",signif(coefs[1],4),"+",
               signif(coefs[2],6),"*",expname,"+",
               signif(coefs[3],6),expname,"^2","\n")
           cat("\n")
         }

         if (degree==3) {
           cat("\tCubic Regression\n\n")
           cat("Equation of Fitted Cubic:\n\n")
           cat("\t",respname,"=",signif(coefs[1],4),"+",
                    signif(coefs[2],4),"*",expname,"+",
                    signif(coefs[3],4),"*",expname,"^2","+",
                    signif(coefs[4],4),"*",expname,"^3","\n")
           cat("\n")
         }
         
         if (degree > 3) {
           cat("\tPolynomial Regression\n\n")
           cat("Equation of Fitted Polynomial:\n\n")
           eq <- paste("\t",respname,"=",signif(coefs[1],4),"+",
                          signif(coefs[2],4),"*",expname,"\n")
           for (i in 3:(degree+1)) {
             eq <- c(eq,paste("\t\t\t\t +",
                              signif(coefs[i],4),"*",expname,"^",i-1,"\n"))
           }
           cat(eq)
           cat("\n")
         }
         
         cat("Residual Standard Error:\ts   =",round(GClm$resid.sterr,4),"\n")
         cat("R^2 (unadjusted):\t\tR^2 =",round(GClm$r.squared,4),"\n")
         
         
         #make data frame with complete cases to suppress warnings in ggplot
         df <- data.frame(GClm$exp,GClm$resp)
         names(df) <- c(expname,respname)
         df <- df[complete.cases(df),]
         
         if (GClm$graph && !GClm$check) {
           xFill <- GClm$xFill
           fitsFill=GClm$fitsFill
           sepredFill <- GClm$sepredFill
           predfr <- data.frame(xFill,fitsFill,
                                fitsFill-sepredFill,
                                fitsFill+sepredFill)
           names(predfr) <- c(expname,respname,"lwr","upr")
           
          
           
           p1 <- ggplot(df, aes_string(x=expname,y=respname))+
             geom_point()+
             stat_smooth(method = "lm", formula = y ~ poly(x, degree,raw=TRUE), size = 1,se=FALSE)+
             xlab(expname)+ylab(respname)
           
           print(p1+geom_ribbon(data=predfr,aes(ymin=lwr,ymax=upr),alpha=0.2))
           
         }
         
         
         if (GClm$check) {
           
           if (length(GClm$exp) < 1000) method <- "loess" else method <- "gam"
           
           title <- paste0("Checking the Model Fit\n(model line/curve is blue; ",method," curve is red)")
           
           p1 <- ggplot(df, aes_string(x=expname,y=respname))+
              ggtitle(title)+
             geom_point()+
             stat_smooth(method = "lm", formula = y ~ poly(x, degree,raw=TRUE), size = 1,se=TRUE)+
             xlab(expname)+ylab(respname) + stat_smooth(method=method,color="red",size=1,se=FALSE)
             
           
           print(p1)
           
         }
         
         
         
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