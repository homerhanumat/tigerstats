#' @title Prediction Function for GC Linear Regression

#' @description Used by generic predict function
#' 
#' @rdname predict.GClm
#' @method predict GClm
#' @usage 
#' \S3method{predict}{GClm}(object,x,...)
#' @param object An object of class GClm
#' @param x value of the predictor variable
#' @param \ldots ignored
#' @return numeric prediction
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' #predict fastest speed driven, for person with GPA=3.0:
#' SpeedModel <- lmGC(fastest~GPA,data=m111survey)
#' predict(SpeedModel,x=3.0)
predict.GClm2 <-function(object,x,...)  {
    model <- object$mod
    expname <- object$expname
    respname <- object$respname
    residse <- object$resid.sterr
    newdf <- data.frame(x)
    names(newdf) <- expname
    prediction <- predict(model,newdata=newdf,se.fit=TRUE)
    predVal <- prediction$fit
    sepred <- sqrt(residse^2+(prediction$se.fit)^2)
    return(cat(paste0("Predict ",respname," is about ",signif(predVal,4),
        ",\ngive or take ",signif(sepred,4)," or so for chance variation.")))
    
  }#end predict.lmGC