#' @title Print Function for GC Polynomial Regression

#' @description Utility print function
#' @keywords internal
#' 
#' @rdname print.polyGC
#' @method print polyGC
#' @usage 
#' \S3method{print}{polyGC}(x,...)
#' @param x an object of class polyGC
#' @param \ldots ignored
#' @return graphical output and output to console
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
print.polyGC <-function(x,...)  {
  
        length_out <- 1000 #for plotting fitted curve
  
         polyGC <- x
         degree <- polyGC$degree

         respname <- polyGC$respname
         expname <- polyGC$expname
         
#          if (degree==2) {
#            cat("\tQuadratic Regression\n\n")
#            cat("Equation of Fitted Parabola:\n\n")
#            cat(paste0("\t",respname,"=",signif(coefs[1],4)," + ",
#                signif(coefs[2],4),"*",expname," + " ,
#                signif(coefs[3],4),"*",expname,"^2","\n"))
#            cat("\n")
#          }
# 
#          if (degree==3) {
#            cat("\tCubic Regression\n\n")
#            cat("Equation of Fitted Cubic:\n\n")
#            cat(paste0("\t",respname," = ",signif(coefs[1],4)," + ",
#                     signif(coefs[2],4),"*",expname," + ",
#                     signif(coefs[3],4),"*",expname,"^2"," + ",
#                     signif(coefs[4],4),"*",expname,"^3","\n"))
#            cat("\n")
#          }
#          
#          if (degree > 3) {
#            cat("\tPolynomial Regression\n\n")
#            cat("Equation of Fitted Polynomial:\n\n")
#            eq <- paste0("\t",respname," = ",signif(coefs[1],4)," + ",
#                           signif(coefs[2],4),"*",expname,"\n")
#            for (i in 3:(degree+1)) {
#              eq <- c(eq,paste0("\t\t\t\t + ",
#                               signif(coefs[i],4),"*",expname,"^",i-1,"\n"))
#            }
#            cat(eq)
#            cat("\n")
#          }
         
         cat(paste0("Polynomial Regression, Degree = ",degree,"\n\n"))
         cat("Residual Standard Error:\ts   =",round(polyGC$resid.sterr,4),"\n")
         cat("R^2 (unadjusted):\t\tR^2 =",round(polyGC$r.squared,4),"\n")
         
         
         #make data frame with complete cases to suppress warnings in ggplot RE missing data
         df <- data.frame(polyGC$exp,polyGC$resp)
         names(df) <- c(expname,respname)
         df <- df[complete.cases(df),]
         
         exp <- df[,expname]
         resp <- df[,respname]

xFill <- seq(min(exp),max(exp),length.out=length_out)
meanX <- polyGC$meanX
sdX <- polyGC$sdX

xFillCent <- (xFill - meanX)/sdX
newdf <- data.frame(xFillCent)
names(newdf) <- expname

mod <- polyGC$mod
fitsFill <- predict(mod,newdata=newdf)

predfr <- data.frame(xFill,fitsFill)
names(predfr) <- c(expname,respname)
        
         
         if (polyGC$graph && !polyGC$check) {
           
 
           
           
           if (degree == 2) {
             title <- paste0("Scatterplot with quadratic fit")
           }
           
           if (degree == 3) {
             title <- paste0("Scatterplot with cubic fit")
           }
           
           if (degree >= 4) {
             title <- paste0("Scatterplot with degree-",degree," fit")
           }
           
           p1 <- ggplot2::ggplot(df,ggplot2::aes_string(x=expname,y=respname))+
             ggplot2::ggtitle(title)+
             ggplot2::geom_point()+
             ggplot2::geom_point(data=predfr,ggplot2::aes_string(x=expname,y=respname),col="blue",size=1)+
             ggplot2::xlab(expname)+ggplot2::ylab(respname)
           
           suppressWarnings(print(p1))
           
         }
         
         
         if (polyGC$check) {
           
           if (length(polyGC$exp) < 1000) method <- "loess" else method <- "gam"
           
           title <- paste0("Checking the Model Fit\n(Model is blue; ",method,
                           " curve is red;\n95%-confidence band for ",method," curve is included)")
           
           p1 <- ggplot2::ggplot(df, ggplot2::aes_string(x=expname,y=respname))+
              ggplot2::ggtitle(title)+
             ggplot2::geom_point()+
             ggplot2::stat_smooth(method=method,color="red",size=1,se=TRUE)+
             ggplot2::geom_point(data=predfr,ggplot2::aes_string(x=expname,y=respname),col="blue",size=1)+
             ggplot2::xlab(expname)+ggplot2::ylab(respname)
             
           
           suppressWarnings(print(p1))
           
         }
         
         
  }#end print.polyGC