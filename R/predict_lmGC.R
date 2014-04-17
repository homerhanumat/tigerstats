#' @title Prediction Function for GC Linear Regression

#' @description Used by generic predict function
#' @keywords internal
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
predict.GClm<-function(object,x,...)  {
    model <- object
    return(model$intercept+model$slope*x)      
  }#end predict.lmGC