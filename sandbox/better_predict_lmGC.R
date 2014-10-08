#' @title Prediction Function for GC Linear Regression

#' @description Used by generic predict function
#' 
#' @rdname predict.GClm
#' @method predict GClm
#' @usage 
#' \S3method{predict}{GClm}(object,x,level=NULL,...)
#' @param object An object of class GClm
#' @param x value of the predictor variable
#' @pqram level desired level of prediction interval
#' @param \ldots ignored
#' @return numeric prediction
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' #predict fastest speed driven, for person with GPA=3.0:
#' SpeedModel <- lmGC(fastest~GPA,data=m111survey)
#' predict(SpeedModel,x=3.0)
#' #include prediction interval:
#' predict(SpeedModel,x=3.0,level=0.95)
predict.GClm <-function(object,x,level=NULL,...)  {

    expname <- object$expname
    respname <- object$respname
    exp <- object$exp
    model <- object$mod

    if (!is.null(level)) {
      if (level <=0 || level >= 1) {
        stop("Level must be a number between 0 and 1")
      }
    }
    
    residse <- object$resid.sterr
    
    newdf <- data.frame(x)
    names(newdf) <- expname
    
    prediction1 <- predict(model,newdata=newdf,se.fit=TRUE)
    predVal <- prediction1$fit
    sepred <- sqrt(residse^2+(prediction1$se.fit)^2)
    
    cat(paste0("Predict ",respname," is about ",signif(predVal,4),
        ",\ngive or take ",signif(sepred,4)," or so for chance variation.\n\n"))
    
    if (!is.null(level)) {
    
    prediction2 <- suppressWarnings(predict(model,newdata=newdf,interval="prediction",level=level))
    lower <- prediction2[2]
    upper <- prediction2[3]
    cat(paste0(100*level,"%-prediction interval:\n"))
    int <- c(lower,upper)
    cat(sprintf("%-10s%-20s%-20s","","lower.bound","upper.bound"),"\n")
    cat(sprintf("%-10s%-20f%-20f","",int[1],int[2]),"\n\n")
    
    }
    
  }#end predict.lmGC