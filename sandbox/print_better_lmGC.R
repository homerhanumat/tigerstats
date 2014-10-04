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
           cat(paste0("\t",respname,"=",signif(coefs[1],4)," + ",
               signif(coefs[2],4),"*",expname," + " ,
               signif(coefs[3],4),"*",expname,"^2","\n"))
           cat("\n")
         }

         if (degree==3) {
           cat("\tCubic Regression\n\n")
           cat("Equation of Fitted Cubic:\n\n")
           cat(paste0("\t",respname," = ",signif(coefs[1],4)," + ",
                    signif(coefs[2],4),"*",expname," + ",
                    signif(coefs[3],4),"*",expname,"^2"," + ",
                    signif(coefs[4],4),"*",expname,"^3","\n"))
           cat("\n")
         }
         
         if (degree > 3) {
           cat("\tPolynomial Regression\n\n")
           cat("Equation of Fitted Polynomial:\n\n")
           eq <- paste0("\t",respname," = ",signif(coefs[1],4)," + ",
                          signif(coefs[2],4),"*",expname,"\n")
           for (i in 3:(degree+1)) {
             eq <- c(eq,paste0("\t\t\t\t + ",
                              signif(coefs[i],4),"*",expname,"^",i-1,"\n"))
           }
           cat(eq)
           cat("\n")
         }
         
         cat("Residual Standard Error:\ts   =",round(GClm$resid.sterr,4),"\n")
         cat("R^2 (unadjusted):\t\tR^2 =",round(GClm$r.squared,4),"\n")
         
         
         #make data frame with complete cases to suppress warnings in ggplot RE missing data
         df <- data.frame(GClm$exp,GClm$resp)
         names(df) <- c(expname,respname)
         df <- df[complete.cases(df),]
         
         
         # now experiment with centering
          
         xMean <- mean(df[,expname])
         xSD <- sd(df[,expname])
         xCent <- (df[,expname]-xMean)/xSD
         
         df[,expname] <- xCent
        
         
         if (GClm$graph && !GClm$check) {
           xFill <- GClm$xFill
           
           #experiment with re-scaling
           xFillCent <- (xFill - xMean)/xSD
           fitsFill=GClm$fitsFill
           predfr <- data.frame(xFillCent,as.vector(fitsFill[,1]),
                                as.vector(fitsFill[,2]),
                                as.vector(fitsFill[,3]))
           names(predfr) <- c(expname,respname,"lwr","upr")
           
           if (degree == 1) {
             title <- paste0("Scatterplot with linear fit\nand 95%-prediction band")
           }
           
           if (degree == 2) {
             title <- paste0("Scatterplot with quadratic fit\nand 95%-prediction band")
           }
           
           if (degree == 3) {
             title <- paste0("Scatterplot with cubic fit\nand 95%-prediction band")
           }
           
           if (degree >= 4) {
             title <- paste0("Scatterplot with degree-",degree," fit\nand 95%-prediction band")
           }
           
           p1 <- ggplot2::ggplot(df)+
             ggtitle(title)+
             geom_point(df, aes_string(x=expname,y=respname))+
             +geom_point(data=predfr,aes_string(x=expname,y=respname))+
             #stat_smooth(method = "lm", formula = y ~ poly(x, degree,raw=TRUE), size = 1,se=FALSE)+
             xlab(expname)+ylab(respname)+
             geom_ribbon(data=predfr,aes(ymin=lwr,ymax=upr),alpha=0.2)
           
           suppressWarnings(print(p1))
           
         }
         
         
         if (GClm$check) {
           
           if (length(GClm$exp) < 1000) method <- "loess" else method <- "gam"
           
           title <- paste0("Checking the Model Fit\n(Model is blue; ",method,
                           " curve is red;\n95%-confidence band for model is included)")
           
           p1 <- ggplot2::ggplot(df, aes_string(x=expname,y=respname))+
              ggtitle(title)+
             geom_point()+
             stat_smooth(method = "lm", formula = y ~ poly(x, degree,raw=TRUE), size = 1,se=TRUE)+
             xlab(expname)+ylab(respname) + stat_smooth(method=method,color="red",size=1,se=FALSE)
             
           
           suppressWarnings(print(p1))
           
         }
         
         
  }#end print.GClm