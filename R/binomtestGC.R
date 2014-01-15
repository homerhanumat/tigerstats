#' @title Exact Procedures for a Single Proportion

#' @description Wrapper for binom.test in package \code{stats}.  Employs the binomial distribution 
#' in inferential procedures for a single proportion.
#' 
#' @rdname binom.testGC
#' @usage binom.testGC(x,n=numeric(),p=NULL,data,alternative="two.sided",
#'                          success="yes",conf.level=0.95,graph=FALSE)
#' @param x Either a formula or a numeric vector.  If formula, it must be of the form ~x
#' indicating the single variable under study.  When summary data are provided, x is a numeric vector of 
#' success counts.
#' @param n When not empty, this is a numeric vector giving the size of the sample.
#' @param p Specifies Null Hypothesis value for population proportion.  If not set, no test is performed.
#' @param data Data frame that supplies the variable x.
#' @param alternative "two.sided" requests computation of a two-sided P-value;  
#' other possible values are "less" and "greater".
#' @param success  When x is a formula, this argument indicates which value of variable x is being counted as a success.  
#' When working with formula-data input the value of this parameter MUST be set, even when the variable has only
#' two values.
#' @param conf.level Number between 0 and 1 indicating the confidence-level of the interval supplied.
#' @param graph If TRUE, plot graph of P-value.  Ignored if no test is performed.
#' @return Output to console.  Future versions may return an object, and include a print method.
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' data(m111survey)
#' binom.testGC(~sex,data=m111survey,success="female") #confidence interval only
#' 
#' binom.testGC(~sex,data=m111survey,success="female",p=0.5) #test included
#' 
#' #Summary data:
#' #In one sample, 40 successes in 100 trials.  Testing whether p = 0.45.
#' binom.testGC(40,100,p=0.45)
binom.testGC <-
  function(x,n=numeric(),
           p=NULL,data,
           alternative="two.sided",
           success="yes",
           conf.level=0.95,
           graph=FALSE)  { 
    
    if (is(x,"formula"))  {
      
      prsd <- ParseFormula(x)
      pullout <- as.character(prsd$rhs)
      
      if (length(pullout) > 1) stop("Incorrect formula")
      
      varname <- pullout[1]
      variable <- data[,varname]
      TallyTable <- xtabs(~variable)
      
      if (!(success %in% unique(variable))) {
        stop("No sucesses found.  Did you fail to specify success correctly?")
      }
      successes <- TallyTable[success]
      trials <- sum(TallyTable)
      
    }
      
    
    if (!is(x,"formula")) {  #we have summary data
      
        successes <- x
        trials <- n
        
    }
    
    if (is.null(p))  { #no test
      
      res <- stats::binom.test(successes,trials,p=0.5,conf.level=conf.level)
    } else res <- stats::binom.test(successes,trials,p=p,alternative=alternative,conf.level=conf.level)
    
    #now for the output
   
    cat("\nExact Binomial Procedures for a Single Proportion p:\n")
    if (is(x,"formula")) {
      cat("\tVariable under study is",varname,"\n")
    } else cat("\tResults based on Summary Data\n")
        
    cat("\n\n")
    cat("Descriptive Results: ",successes,"successes in",trials,"trials\n\n")
    
p.hat <- successes/trials
se.phat <- sqrt(p.hat*(1-p.hat)/trials)
        
    cat("Inferential Results:\n\n")
    cat("Estimate of p:\t",p.hat,"\n")
    cat("SE(p.hat):\t",se.phat,"\n\n")
    cat(conf.level*100,"% Confidence Interval for p:\n\n",sep="")
    int <- res$conf.int
    cat(sprintf("%-10s%-20s%-20s","","lower.bound","upper.bound"),"\n")
    cat(sprintf("%-10s%-20f%-20f","",int[1],int[2]),"\n\n")
    
    if (!is.null(p)) {
      
      cat("Test of Significance:\n\n")
      symbol <- switch(alternative,
                       less="<",
                       greater=">",
                       two.sided="!=")
      cat("\tH_0:  p =",p,"\n")
      cat("\tH_a:  p",symbol,p,"\n\n")
      cat("\tP-value:\t\tP =",res$p.value,"\n")
      
     if (graph)   {  
        switch(alternative,
               less=invisible(pbinomGC(successes,size=trials,p=p,region="below",graph=T)),
               greater=invisible(pbinomGC(successes-1,size=trials,p=p,region="above",graph=T)),
               two.sided=warning("No graph is provided for two-sided test")
        )
      }
      
    }
    
  }#end binom.testGC