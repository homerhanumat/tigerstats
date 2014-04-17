#' Parse formulas
#'
#' utility for extracting portions of formulas.
#'
#' @rdname ParseFormula
#' @keywords internal
#' @usage ParseFormula(formula,...)
#' @param formula, a formula
#' @param \dots additional arguments, should folks decide to add them someday
#' @return an object of class \code{parsedFormula}, used to compute on the language
#' @author Inspired by similar function in package \code{mosaic}.  
#' Included in this package to reduce dependency.
#' @export
ParseFormula <- function(formula, ...) {
  op <- formula[[1]]
  condition <- NULL
  if (length(formula) == 2) {
    rhs <- formula[[2]]
    lhs <- NULL
  } else if (length(formula) == 3) {
    rhs <- formula[[3]]
    lhs <- formula[[2]]
  } else {
    stop('Invalid formula type.')
  }
  
  if (inherits(rhs, "call") && rhs[[1]] == '|') {
    condition <- rhs[[3]]
    rhs <- rhs[[2]]
  }
  return( structure(list(op=op,lhs=lhs,rhs=rhs,condition=condition), class='parsedFormula') )
}