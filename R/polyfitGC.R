#' @title Polynomial Regression

#' @description Regression analysis (one numerical predictor variable) with simplified output.
#'   Wrapper function for \code{lm} in package \code{stats}.
#' 
#' @rdname polyfitGC
#' @usage polyfitGC(form,data=parent.frame(),degree=2,graph=TRUE,check=FALSE)
#' @param form formula of form y~x, both variables numeric
#' @param data dataframe supplying y and x above.  If one or more of the variables is not in data, then
#' they will be searched for in the parent environment.
#' @param degree desired degree of polynomial (for degree 1 use lmgC)
#' @param graph Produce scatterplot with fitted ploynomial.
#' @param check Asks to produce a lowess or gam curve with approximate 95%-confidence band.  If the
#' fitted line wanders outside the band, then perhaps a linear fit is not appropriate.
#' @return A list of class "polyGC".  Elements that may be queried include
#' "s" (residual standard error) and "R^2" (unadjusted).
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' #To study the relationship between two numerical variables:
#' polyfitGC(mpg~wt,data=mtcars,degree=2,graph=TRUE)
#' #check the second-fdegree fit:
#' polyfitGC(mpg~wt,data=mtcars,degree=2,check=TRUE)
polyfitGC <-function(form,data=parent.frame(),degree=2,graph=TRUE,check=FALSE)  {
  
  if (degree==1) stop("For linear fit, use lmGC().")
  
  prsd <- ParseFormula(form)
  respname <- as.character(prsd$lhs)
  expname <- as.character(prsd$rhs)
  
  if (length(expname)>1) stop("Only one predictor variable permitted")
  
  resp <- simpleFind(varName=respname,data=data)
  exp <- simpleFind(varName=expname,data=data)
  
  
  if (!is(resp,"numeric")) stop("Response variable must be numerical")
  if (!is(exp,"numeric")) stop("Predictor variable must be numerical")
  
  #get the numbers from stats::lm
  
  # first center the x-values to minimize numerical issues
  
  meanX <- mean(exp,na.rm=TRUE)
  sdX <- sd(exp,na.rm=TRUE)
  expCent <- (exp - meanX)/sdX
  
  df <- data.frame(expCent,resp)
  names(df) <- c(expname,respname)
  form <- as.formula(paste0(respname,"~ poly(",expname,",",degree,", raw=TRUE)"))
  polyMod <- lm(form, data=df)
  results <- summary(polyMod)
  

  residse <- results$sigma
  
  #Collect what we need for our print function:
  results2 <- list(expname=expname,
                   respname=respname,
                   exp=exp,
                   resp=resp,
                   residuals=polyMod$residuals,
                   coefficients=results$coefficients,
                   r.squared=results$r.squared,
                   resid.sterr=residse,
                   graph=graph,
                   check=check,
                   degree=degree,
                   meanX=meanX,
                   sdX=sdX,
                   mod=polyMod)
  
  class(results2) <- "polyGC"
  return(results2)
  
}#end polyfitGC