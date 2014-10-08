#' @title Linear Regression

#' @description Regression analysis (one numerical predictor variable) with simplified output.
#'   Wrapper function for \code{lm} in package \code{stats}.
#' 
#' @rdname lmGC
#' @usage lmGC(form,data=parent.frame(),graph=FALSE,check=FALSE)
#' @param form formula of form y~x, both variables numeric
#' @param data dataframe supplying y and x above.  If one or more of the variables is not in data, then
#' they will be searched for in the parent environment.
#' @param graph Produce scatterplot with fitted ploynomial, together with prediction standard error bands
#' @param check Asks to produce a lowess or gam curve with approximate 95%-confidence band.  If the
#' fitted line wanders outside the band, then perhaps a linear fit is not appropriate.
#' @return A list of class "GClm".  Elements that may be queried include "slope", "intercept",
#' "s" (residual standard error), "R^2" (unadjusted).
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' #To study the relationship between two numerical variables:
#' lmGC(fastest~GPA,data=m111survey,graph=TRUE)
lmGC <-function(form,data=parent.frame(),graph=FALSE,check=FALSE)  {
  
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
  form <- as.formula(paste0(respname,"~",expname))
  resultslm <- lm(form, data=df)
  results <- summary(resultslm)
  
  
  residse <- results$sigma
  
  #old code fomr when I wanted to make "naive" prediction intervals
  #sepredFill <- sqrt(residse^2+(fitsFill$se.fit)^2)
  
  #Collect what we need for our print function:
  results2 <- list(expname=expname,
                   respname=respname,
                   exp=exp,
                   resp=resp,
                   residuals=resultslm$residuals,
                   coefficients=results$coefficients,
                   r.squared=results$r.squared,
                   resid.sterr=residse,
                   graph=graph,
                   check=check,
                   mod=resultslm)
  
  class(results2) <- "GClm"
  return(results2)
  
}#end GClm