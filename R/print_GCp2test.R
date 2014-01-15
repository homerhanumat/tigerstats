#' @title Print Function for GC Proportions Test (Two-Sample)

#' @description Utility print function
#' 
#' @rdname print.gcp2test
#' @method print gcp2test
#' @usage 
#' \S3method{print}{gcp2test}(x,...)
#' @param x An object of class gcp2test.
#' @param \ldots ignored
#' @return Output to the console.
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @export
print.gcp2test <- function(x,...)  {
  gcp2test <- x
  cat("\n\nInferential Procedures for the Difference of Two Proportions p1-p2:\n")
  if (!is.na(gcp2test$explanatory)) {
  cat("\t",gcp2test$response,"grouped by",gcp2test$explanatory,"\n")
    } else cat("\tResults taken from summary data.\n")
  cat("\n\n")
  cat("Descriptive Results:\n\n")
  
  tab <- gcp2test$SummTab
  print(tab)
  
  cat("\n")
  cat("\n")
  
  checker <- min(tab[1,1],tab[2,1],tab[1,2]-tab[1,1],tab[2,2]-tab[2,1])
  if (checker < 10) {
    cat("WARNING:  In at least one of the two groups\n",
      "number of successes or number of failures is below 10.\n",
      "The normal approximation for confidence intervals\n",
      "and P-value may be unreliable.\n\n",sep="")
  }
  
  
  cat("Inferential Results:\n\n")
  cat("Estimate of p1-p2:\t",tab[1,3]-tab[2,3],"\n")
  cat("SE(p1.hat - p2.hat):\t",gcp2test$se,"\n\n")
  cat(gcp2test$conf.level*100,"% Confidence Interval for p1-p2:\n\n",sep="")
  int <- gcp2test$interval
  cat(sprintf("%-10s%-20s%-20s","","lower.bound","upper.bound"),"\n")
  cat(sprintf("%-10s%-20f%-20f","",int[1],int[2]),"\n\n")
  
  if(gcp2test$p.value) {
    
  cat("Test of Significance:\n\n")
  symbol <- switch(gcp2test$alternative,
                   less="<",
                   greater=">",
                   two.sided="!=")
  cat("\tH_0:  p1-p2 = 0\n")
  cat("\tH_a:  p1-p2",symbol,"0\n\n")
  cat("\tTest Statistic:\t\tz =",gcp2test$statistic,"\n")
  cat("\tP-value:\t\tP =",gcp2test$p.value,"\n")
  
  Grapher <- function(stat,alt) {
    rstat <- round(stat,2)
    switch(alt,
           less=invisible(pnormGC(rstat,region="below",graph=T)),
           greater=invisible(pnormGC(rstat,region="above",graph=T)),
           two.sided=invisible(pnormGC(c(-abs(rstat),abs(rstat)),region="outside",graph=T))
    )
  }
  
  if (gcp2test$graph) {
    Grapher(stat=gcp2test$statistic,alt=gcp2test$alternative)
  }
  
  }
  
  }