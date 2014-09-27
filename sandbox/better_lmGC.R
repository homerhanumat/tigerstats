#' @title Linear Regression

#' @description Regression analysis (one numerical predictor variable) with simplified output.
#'   Wrapper function for \code{lm} in package \code{stats}.
#' 
#' @rdname lmGC
#' @usage lmGC(form,data=parent.frame(),graph=FALSE,diag=FALSE,degree=1)
#' @param form formula of form y~x, both variables numeric
#' @param data dataframe supplying y and x above.  If one or more of the variables is not in data, then
#' they will be searched for in the parent environment.
#' @param graph produce scatterplot with regression line
#' @param diag produces diagnostic plots:  density plot of residuals, and residuals vs. fits
#' @param degree Degree of polynomial to fit to default.  Default is linear fit.
#' @return A list of class "GClm".  Elements that may be queried include "slope", "intercept",
#' "s" (residual standard error), "R^2" (unadjusted).
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' #To study the relationship between two numerical variables:
#' lmGC(fastest~GPA,data=m111survey,graph=TRUE)
lmGC2 <-function(form,data=parent.frame(),graph=FALSE,diag=FALSE,degree=1)  {
  
  prsd <- ParseFormula(form)
  respname <- as.character(prsd$lhs)
  expname <- as.character(prsd$rhs)
  
  if (length(expname)>1) stop("Only one predictor variable permitted")
  
  resp <- simpleFind(varName=respname,data=data)
  exp <- simpleFind(varName=expname,data=data)
  
  if (!is(resp,"numeric")) stop("Response variable must be numerical")
  if (!is(exp,"numeric")) stop("Predictor variable must be numerical")
  
  #get the numbers from stats::lm
  df <- data.frame(exp,resp)
  names(df) <- c(expname,respname)
  form <- as.formula(paste0(respname,"~poly(",expname,",",degree,",raw=TRUE)"))
  resultslm <- lm(form, data=df)
  results <- summary(resultslm)
  
  
  # for graphing
  n <- 500 #desired number of poonts to make fitting curve and se.fits
  
  xFill <- seq(min(exp),max(exp),length.out=n)
  newdf <- data.frame(xFill)
  names(newdf) <- expname
  fitsFill <- predict(resultslm,newdata=newdf,se.fit=TRUE)
  residse <- results$sigma
  sepredFill <- sqrt(residse^2+(fitsFill$se.fit)^2)
  
  #Collect what we need for our print function:
  results2 <- list(expname=expname,
                   respname=respname,
                   exp=exp,
                   resp=resp,
                   residuals=resultslm$residuals,
                   fitted.values=resultslm$fitted.values,
                   coefficients=results$coefficients,
                   r.squared=results$r.squared,
                   resid.sterr=residse,
                   xFill=xFill,
                   fitsFill=fitsFill$fit,
                   sepredFill=sepredFill,
                   degree=degree,
                   graph=graph,diag=diag,
                   mod=resultslm)
  
  class(results2) <- "GClm2"
  return(results2)
  
}#end GClm

# for easy dev
simpleFind <- function(varName,data) {
  tryCatch({get(varName,envir=as.environment(data))},
           error=function(e) {
             get(varName,inherits=T)
           }
  )
  
}