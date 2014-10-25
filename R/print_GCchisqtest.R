#' @title Print Function for chisqtestGC

#' @description Utility print function
#' @keywords internal
#' 
#' @rdname print.GCchisqtest
#' @method print GCchisqtest
#' @usage 
#' \S3method{print}{GCchisqtest}(x,...)
#' @param x An object of class GCchisqtest.
#' @param \ldots ignored
#' @return Output to the console and to the plot window
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @export
print.GCchisqtest <- function(x,...)  {
  res <- x
  simulate.p.value <- res$simulate.p.value
  B <- res$B
  sims <- res$sims
  verbose <- res$verbose
  type <- res$type
  observed <- res$observed
  expected <- res$expected
  residuals <- res$residuals
  p.value <- res$p.value
  method <- res$method
  statistic <- res$statistic
  graph <- res$graph

  
  if (simulate.p.value==FALSE) {
    cat(method,"\n\n")
  }
  
  if (simulate.p.value=="fixed") {
    cat("Pearson's chi-squared test with simulated p-value, fixed effects\n\t (based on",B,"resamples)\n\n")
  }
  
  if (simulate.p.value=="random") {
    cat("Pearson's chi-squared test with simulated p-value, random effects\n\t (based on",B,"resamples)\n\n")
  }
  
  if (simulate.p.value==TRUE) {
    cat("Pearson's chi-squared test with simulated p-value \n\t (based on",B,"resamples)\n\n")
  }
  
  if (verbose) {#print some tables
    
    if (type=="goodness"){
      nice <- cbind(observed,round(expected,2),round(residuals^2,2))
      colnames(nice) <- c("Observed counts","Expected by Null","Contr to chisq stat")
      print(nice)
    }
    
    if (type=="association"){
      cat("Observed Counts:\n")
      print(observed)
      cat("\n")
      cat("Counts Expected by Null:\n")
      print(round(expected,2))
      cat("\n")
      cat("Contributions to the chi-square statistic:\n")
      print(round(residuals^2,2))
    }
    cat("\n\n")
  }#end of verbose table printing
  
  #next, statistic, degrees of freedom and P-value
  cat("Chi-Square Statistic =",round(statistic,4),"\n")
  tab <- observed
  if (length(dim(tab))==1)  {df <- nrow(tab)-1} else {df <- (nrow(tab)-1)*(ncol(tab)-1)}
  cat("Degrees of Freedom of the table =",df,"\n")
  cat("P-Value =",round(p.value,4),"\n\n")
  
  #warn if no simulation and any expected cell counts are below 5
  if (min(expected) <5 && simulate.p.value==FALSE){
    warning("Some expected cell counts are low:\n\tthe approximation of the P-value may be unreliable.\n\tConsider using simulation.")
  }
  
  #finally, the graph
  if (graph && !(simulate.p.value==FALSE)) {
    hist(sims,xlab="Chi-Square Resamples",freq=TRUE,
         main=paste("Distribution of",B,"Resamples"),col="lightblue")
    abline(v=statistic,col="red",lwd=2)
  }
  
  if (graph==TRUE && simulate.p.value==FALSE) {
    invisible(pchisqGC(statistic,region="above",df=df,graph=T))
  }

  }