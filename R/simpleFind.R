#' @title Get a variable from its name

#' @description Primitive utility function, for writing functions that handle formula input.  simpleFind
#' looks first in the environment associated with the data argument.  If nothing is found, it looks
#' in the parent environment, and so on up the chain.  The intent is to allow use of formula constructed
#' from names of variables that may not appear in the data frame of interest, but which are present in the 
#' caller's environment (usually the Global Environment).  Functions that use formulas now are more flexible
#' in an interactive context.
#' 
#' @rdname simpleFind
#' @usage simpleFind(varName,data)
#' @param varName Character string giving the name of the variable to be searched for.
#' @param data Usually a data frame that supplies the some or all of the variables for a formula 
#' that is has been passed to the calling function.
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
simpleFind <- function(varName,data) {
  tryCatch({
    get(varName,envir=as.environment(data))
  }, warning = function(w) {
    #warning-handler-code maybe someday
  }, error = function(e) {
    get(varName,inherits=T)
  }, finally = {
    #cleanup-code maybe someday
  }
  )
}