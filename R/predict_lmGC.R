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
predict.GClm<-function(object,x,...)  {
    model <- object
    return(model$intercept+model$slope*x)      
  }#end predict.lmGC