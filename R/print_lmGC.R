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
print.GClm <-function(x,...)  {
         GClm <- x
         coefs <- GClm$coefficients
         exp <- GClm$exp
         resp <- GClm$resp
         respname <- GClm$respname
         expname <- GClm$expname
         cat("\n") 
         
         cat("\tLinear Regression\n\n")
         cat("Correlation coefficient r = ",signif(cor(GClm$exp,GClm$resp,use="na.or.complete"),4),"\n\n")
         cat("Equation of Regression Line:\n\n")
         cat("\t",respname,"=",round(coefs[1],4),"+",round(coefs[2],4),"*",
                expname,"\n")
         cat("\n")
         
         cat("Residual Standard Error:\ts   =",round(GClm$resid.sterr,4),"\n")
         cat("R^2 (unadjusted):\t\tR^2 =",round(GClm$r.squared,4),"\n")
         
         
         #make data frame with complete cases to suppress warnings in ggplot RE missing data
         df <- data.frame(GClm$exp,GClm$resp)
         names(df) <- c(expname,respname)
         df <- df[complete.cases(df),]
        
         
         if (GClm$graph && !GClm$check) {
           
           title <- paste0("Scatterplot with linear fit")
           
           
           p1 <- ggplot2::ggplot(df,ggplot2::aes_string(x=expname,y=respname))+
             ggplot2::ggtitle(title)+
             ggplot2::geom_point()+
             ggplot2::stat_smooth(method = "lm",size = 1,se=FALSE)+
             ggplot2::xlab(expname)+ggplot2::ylab(respname)
           
           suppressWarnings(print(p1)) #suppress just in case Hadley has more friendly advice
           
         }
         
         
         if (GClm$check) {
           
           if (length(GClm$exp) < 1000) method <- "loess" else method <- "gam"
           
           title <- paste0("Checking the Model Fit\n(Model is blue; ",method,
                           " curve is red;\n95%-confidence band for curve included)")
           
           p1 <- ggplot2::ggplot(df, ggplot2::aes_string(x=expname,y=respname))+
              ggplot2::ggtitle(title)+
             ggplot2::geom_point()+
             ggplot2::stat_smooth(method = "lm", size = 1,se=FALSE)+
             ggplot2::xlab(expname)+ggplot2::ylab(respname) + 
             ggplot2::stat_smooth(method=method,color="red",size=1,se=TRUE)
             
           
           suppressWarnings(print(p1))
           
         }
         
         
  }#end print.GClm