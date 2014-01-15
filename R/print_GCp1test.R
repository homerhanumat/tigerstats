#' @title Print Function for GC Proportion Test (One-Sample)

#' @description Utility print function
#' 
#' @rdname print.gcp1test
#' @method print gcp1test
#' @usage 
#' \S3method{print}{gcp1test}(x,...)
#' @param x An object of class gcp1test.
#' @param \ldots ignored
#' @return Output to the console.
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @export 
print.gcp1test <- function(x,...)  {
  gcp1test <- x
  cat("\n\nInferential Procedures for a Single Proportion p:\n")
  if (!is.na(gcp1test$variable)) {
  cat("\tVariable under study is",gcp1test$variable,"\n")
  } else cat("\tResults based on Summary Data\n")
  if (gcp1test$correct==TRUE) {
    cat("\tContinuity Correction Applied to Test Statistic\n")
  }
  cat("\n\n")
  cat("Descriptive Results:\n\n")
  
  tab <- gcp1test$SummTab
  print(tab)
  
  cat("\n")
  cat("\n")
  
  checker <- min(tab[1,1],tab[1,2]-tab[1,1])
  if (checker < 10) {
    cat("WARNING:  Either the number of successes or \nthe number of failures is below 10.\nThe normal approximation for confidence intervals\nand P-value may be unreliable\n\n\n")
  }
  
  
    
  cat("Inferential Results:\n\n")
  cat("Estimate of p:\t",tab[1,3],"\n")
  cat("SE(p.hat):\t",gcp1test$se,"\n\n")
  cat(gcp1test$conf.level*100,"% Confidence Interval for p:\n\n",sep="")
  int <- gcp1test$interval
  cat(sprintf("%-10s%-20s%-20s","","lower.bound","upper.bound"),"\n")
  cat(sprintf("%-10s%-20f%-20f","",int[1],int[2]),"\n\n")
  
  if (gcp1test$p.value) {
  
  cat("Test of Significance:\n\n")
  symbol <- switch(gcp1test$alternative,
                   less="<",
                   greater=">",
                   two.sided="!=")
  cat("\tH_0:  p =",gcp1test$p,"\n")
  cat("\tH_a:  p",symbol,gcp1test$p,"\n\n")
  cat("\tTest Statistic:\t\tz =",gcp1test$statistic,"\n")
  cat("\tP-value:\t\tP =",gcp1test$p.value,"\n")
  
  Grapher <- function(stat,alt) {
    rstat <- round(stat,2)
    switch(alt,
           less=invisible(pnormGC(rstat,region="below",graph=T)),
           greater=invisible(pnormGC(rstat,region="above",graph=T)),
           two.sided=invisible(pnormGC(c(-abs(rstat),abs(rstat)),region="outside",graph=T))
    )
  }
  
  if (gcp1test$graph) {
    Grapher(stat=gcp1test$statistic,alt=gcp1test$alternative)
  }
  
  }
  
}