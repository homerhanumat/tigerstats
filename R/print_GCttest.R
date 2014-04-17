#' @title Print Function for ttestGC

#' @description Utility print function
#' @keywords internal
#' 
#' @rdname print.GCttest
#' @method print GCttest
#' @usage 
#' \S3method{print}{GCttest}(x,...)
#' @param x An object of class GCttest.
#' @param \ldots ignored
#' @return Output to the console.
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @export
print.GCttest <- function(x,...)  {
  GCttest <- x
  subm <- GCttest$subm
  mu <- GCttest$mu
  verbose <- GCttest$verbose
  
  odigits <- getOption("digits")
  options(digits=4)
  
  if (subm %in% c("f2e","f2u","s2e","s2u")) {
  cat("\n\nInferential Procedures for the Difference of Two Means mu1-mu2:\n")
  if (subm %in% c("f2u","s2u")) cat("\t(Welch's Approximation Used for Degrees of Freedom)\n")
  }
  
  if (subm %in% c("s1","f1")) {
    cat("\n\nInferential Procedures for One Mean mu:\n") 
  }
  
  if (subm %in% c("fm")) {
    cat("\n\nInferential Procedures for the Difference of Means mu-d:\n") 
  }
  
  if (subm %in% c("f2e","f2u")) {
  cat("\t",GCttest$varnames[2],"grouped by",GCttest$varnames[1],"\n")
    }
  
  if (subm %in% c("fm")) {
    cat("\t",GCttest$varnames[2],"minus",GCttest$varnames[1],"\n")
  }
  
  if (subm %in% c("s2e","s2u")) cat("\tResults from summary data.\n")
  if (verbose) {    
    cat("\n\n")
    cat("Descriptive Results:\n\n")
  
    tab <- GCttest$SummTab
    print(tab,row.names=FALSE)
  
    cat("\n")
    cat("\n")
  
    cat("Inferential Results:\n\n")
  }   
  if (subm %in% c("s2e","s2u","f2e","f2u")) {
    if (verbose) {
      cat("Estimate of mu1-mu2:\t",GCttest$estimate,"\n")
      cat("SE(x1.bar - x2.bar):\t",GCttest$se,"\n\n")
    }
  cat(GCttest$conf.level*100,"% Confidence Interval for mu1-mu2:\n\n",sep="")
  int <- GCttest$interval
  cat(sprintf("%-10s%-20s%-20s","","lower.bound","upper.bound"),"\n")
  cat(sprintf("%-10s%-20f%-20f","",int[1],int[2]),"\n\n")
  
  if (GCttest$p.value) {
  if (verbose) {
  cat("Test of Significance:\n\n")
  symbol <- switch(GCttest$alternative,
                   less="<",
                   greater=">",
                   two.sided="!=")
  cat("\tH_0:  mu1-mu2 =",GCttest$mu,"\n")
  cat("\tH_a:  mu1-mu2",symbol,mu,"\n\n")
  }
  cat("\tTest Statistic:\t\tt =",GCttest$statistic,"\n")
  cat("\tDegrees of Freedom:\t ",GCttest$df,"\n")
  cat("\tP-value:\t\tP =",GCttest$p.value,"\n")
  }
  }
      
  if (subm %in% c("s1","f1")) {
    if (verbose) {
        cat("Estimate of mu:\t",GCttest$estimate,"\n")
        cat("SE(x.bar):\t",GCttest$se,"\n\n")
    }
        cat(GCttest$conf.level*100,"% Confidence Interval for mu:\n\n",sep="")
        int <- GCttest$interval
        cat(sprintf("%-10s%-20s%-20s","","lower.bound","upper.bound"),"\n")
        cat(sprintf("%-10s%-20f%-20f","",int[1],int[2]),"\n\n")
        if (GCttest$p.value) {
        if (verbose) {
        cat("Test of Significance:\n\n")
        symbol <- switch(GCttest$alternative,
                         less="<",
                         greater=">",
                         two.sided="!=")
        cat("\tH_0:  mu =",GCttest$mu,"\n")
        cat("\tH_a:  mu",symbol,mu,"\n\n")
        }
        cat("\tTest Statistic:\t\tt =",GCttest$statistic,"\n")
        cat("\tDegrees of Freedom:\t ",GCttest$df,"\n")
        cat("\tP-value:\t\tP =",GCttest$p.value,"\n")
        }
  }
      
      if (subm %in% c("fm")) { 
        if (verbose)  {
          cat("Estimate of mu-d:\t",GCttest$estimate,"\n")
          cat("SE(d.bar):\t",GCttest$se,"\n\n")
        }
        cat(GCttest$conf.level*100,"% Confidence Interval for mu-d:\n\n",sep="")
        int <- GCttest$interval
        cat(sprintf("%-10s%-20s%-20s","","lower.bound","upper.bound"),"\n")
        cat(sprintf("%-10s%-20f%-20f","",int[1],int[2]),"\n\n")
        if (GCttest$p.value) {
        if (verbose) {
        cat("Test of Significance:\n\n")
        symbol <- switch(GCttest$alternative,
                         less="<",
                         greater=">",
                         two.sided="!=")
        cat("\tH_0:  mu-d =",GCttest$mu,"\n")
        cat("\tH_a:  mu-d",symbol,mu,"\n\n")
        }
        cat("\tTest Statistic:\t\tt =",GCttest$statistic,"\n")
        cat("\tDegrees of Freedom:\t ",GCttest$df,"\n")
        cat("\tP-value:\t\tP =",GCttest$p.value,"\n")
        }
      }
  
  Grapher <- function(stat,alt,df) {
    rstat <- round(stat,2)
    switch(alt,
           less=invisible(ptGC(rstat,region="below",df=df,graph=T)),
           greater=invisible(ptGC(rstat,region="above",df=df,graph=T)),
           two.sided=invisible(ptGC(c(-abs(rstat),abs(rstat)),region="outside",df=df,graph=T))
    )
  }
  
  if (GCttest$graph) {
    Grapher(stat=GCttest$statistic,alt=GCttest$alternative,df=GCttest$df)
  }
  
  options(digits=odigits)

  }