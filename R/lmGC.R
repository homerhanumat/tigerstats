#' @title Linear Regression

#' @description Regression analysis (one numerical predictor variable) with simplified output.
#'   Wrapper function for \code{lm} in package \code{stats}.
#' 
#' @rdname lmGC
#' @usage lmGC(form,data=parent.frame(),graph=FALSE,diag=FALSE)
#' @param form formula of form y~x, both variables numeric
#' @param data dataframe supplying y and x above.  If one or more of the variables is not in data, then
#' they will be searched for in the parent environment.
#' @param graph produce scatterplot with regression line
#' @param diag produces diagnostic plots:  density plot of residuals, and residuals vs. fits
#' @return A list of class "GClm".  Elements that may be queried include "slope", "intercept",
#' "s" (residual standard error), "R^2" (unadjusted).
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' #To study the relationship between two numerical variables:
#' lmGC(fastest~GPA,data=m111survey,graph=TRUE)
lmGC <-function(form,data=parent.frame(),graph=FALSE,diag=FALSE)  {
  
  prsd <- ParseFormula(form)
  respname <- as.character(prsd$lhs)
  expname <- as.character(prsd$rhs)
  
  if (length(expname)>1) stop("Only one predictor variable permitted")
  
  resp <- simpleFind(varName=respname,data=data)
  exp <- simpleFind(varName=expname,data=data)
  
  if (!is(resp,"numeric")) stop("Response variable must be numerical")
  if (!is(exp,"numeric")) stop("Predictor variable must be numerical")
  
  #get the numbers from stats:lm
  results <- summary(lm(resp~exp))
  results.lm <- lm(resp~exp)
  
  #Collect what we need for our print function:
  results2 <- list(expname=expname,
                   respname=respname,
                   exp=exp,
                   resp=resp,
                   residuals=results.lm$residuals,
                   fitted.values=results.lm$fitted.values,
                   intercept=results$coefficients[1],
                   slope=results$coefficients[2],
                   r.squared=results$r.squared,
                   resid.sterr=results$sigma,
                   graph=graph,diag=diag)
  
  class(results2) <- "GClm"
  return(results2)
  
}#end GClm