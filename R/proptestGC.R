#' @title Proportions Procedures

#' @description Employs the normal approximation to perform test for one or two proportions.
#' 
#' @rdname proptestGC
#' @usage proptestGC(x,n=numeric(),p=NULL,data,alternative="two.sided",
#'                          success="yes",first=NULL,conf.level=0.95,correct=TRUE,graph=FALSE,verbose=TRUE)
#' @param x Either a formula or a numeric vector.  If formula, it must be of the form ~x
#' indicating the single variable under study, or of the form ~x+y, in which case x is the explanatory grouping variable
#' (categorical with two values) and y is the response categorical variable with two values.
#' When summary data are provided, x is a numeric vector of success counts.
#' @param n When not empty, this is a numeric vector giving the size of each sample.
#' @param p Specifies Null Hypothesis value for population proportion.  If not set, no test is performed.
#' @param data Data frame that supplies the variables x and y.
#' @param alternative "two.sided" requests computation of a two-sided P-value;  other possible values are "less" and "greater".
#' @param success  When x is a formula, this argument indicates which value of variable x (in case of ~x) or y (in case of ~x+y)
#' is being counted as a success.  When working
#' with formula-data input the value of this parameter MUST be set, even when the variable has only
#' two values.
#' @param first When performing 2-sample procedures, this argument specifies which value of
#' the explanatory variable constitutes the first group.
#' @param conf.level Number between 0 and 1 indicating the confidence-level of the interval supplied.
#' @param correct Applies continuity correction for one-proportion procedures.  It is ignored when
#' when 2-proportions are performed.
#' @param graph If TRUE, plot graph of P-value.
#' @param verbose Indicates how much output goes to the console
#' @return A list, either of class "gcp1test" (one-proportion) or "gcp2test" (two proportions).  
#' Components of this list that may be usefully queried include:  "statistic", "p.value", and "interval".
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' data(m111survey)
#' #2-proportions, formula-data input, 95%-confidence interval only:
#' proptestGC(~sex+seat,data=m111survey,success="1_front")
#' 
#' #For other confidence levels, use argument conf.level.  For 90%-interval for one proportion p:
#' proptestGC(~sex,data=m111survey,success="male",conf.level=0.90)
#' 
#' #one proportion, formula-data input, confidence interval and two-sided test with H_0:  p = 0.33:
#' proptestGC(~seat,data=m111survey,success="1_front",p=0.33)
#' 
#' #Summary data:
#' #In first sample, 23 successes out of 100 trials.  In second sample, 33 out of 110.
#' proptestGC(x=c(23,33),n=c(100,110))
#' 
#' #Summary data:
#' #In one sample, 40 successes in 100 trials.  Testing whether p = 0.45.
#' proptestGC(40,100,p=0.45,correct=TRUE)
#' 
#' #Want less output?  Set argument verbose to FALSE:
#' proptestGC(~sex+seat,data=m111survey,success="1_front",p=0.33,verbose=FALSE)
proptestGC <-
  function(x,n=numeric(),
           p=NULL,data,
           alternative="two.sided",
           success="yes",first=NULL,
           conf.level=0.95,
           correct=TRUE,graph=FALSE,verbose=TRUE)  {
    
    statistic <- FALSE
    p.value <- FALSE #these will get numerical values if a test is to be performed
    
    #Small Utility Function
    GetP <- function(stat,alternative) {
      switch(alternative,
             less=pnorm(stat),
             greater=pnorm(stat,lower.tail=FALSE),
             two.sided=2*pnorm(abs(stat),lower.tail=FALSE))
    }
    
    #small utility function:conf int for one prop
    GetCI1 <- function(est,se,conf.level,alternative) {
      switch(alternative,
             less=c(lower=0,upper=min(1,est+qnorm(conf.level)*se)),
             two.sided=c(lower=max(0,est+qnorm((1-conf.level)/2)*se),
                         upper=min(1,est+qnorm((1-conf.level)/2,lower.tail=FALSE)*se)),
             greater=c(max(0,lower=est+qnorm(1-conf.level)*se),upper=1)
      )   
    }
    
    #utility function:  conf int for 2 diff of 2 props
    GetCI2 <- function(est,se,conf.level,alternative) {
      switch(alternative,
             less=c(lower=-1,upper=min(1,est+qnorm(conf.level)*se)),
             two.sided=c(lower=max(-1,est+qnorm((1-conf.level)/2)*se),
                         upper=min(1,est+qnorm((1-conf.level)/2,lower.tail=FALSE)*se)),
             greater=c(max(-1,lower=est+qnorm(1-conf.level)*se),upper=1)
      )   
    }
    
    #This function handles formula-data input:
    proptestGC.form <- function(form,data,success="yes",
                                 alternative="two.sided",
                                 conf.level=0.95,
                                 correct=TRUE )  {
      

      
      prsd <- ParseFormula(form)
      
      pullout <- as.character(prsd$rhs)
      
      if (length(pullout)==3) {#we have a bona fide formula
        expname <- as.character(prsd$rhs)[2]
        respname <- as.character(prsd$rhs)[3]
        explanatory <- data[,expname]
        
        #Check to see that explanatory variable has exactly two values:
        if (length(unique(explanatory)) != 2) stop(paste(expname,"must have exactly two values."))
        
        
        response <- data[,respname]
        
        if (!(success %in% unique(response))) {
          stop("No sucesses found.  Did you fail to specify success correctly?")
        }
        
        TwoWayTable <- xtabs(~explanatory+response)
        
        success.tab <- TwoWayTable[,success]
        
        
        #Count all values that are not success as failure:
        failure.names <- colnames(TwoWayTable)[colnames(TwoWayTable)!=success]
        failure.tab <- as.matrix(TwoWayTable[,failure.names])
        comb.failure.tab <- rowSums(failure.tab)
        
        twobytwotab <- cbind(success.tab,comb.failure.tab)
        rownames(twobytwotab) <- NULL
        colnames(twobytwotab) <- NULL
        
        flip <- FALSE
        if (!is.null(first)) {
          if (!(first %in% unique(explanatory))) {
            stop(paste(first,"is not a value of",explanatory))}
          if (sort(unique(explanatory))[1]!=first) flip <- TRUE
        }
        
        tttp <- prop.table(twobytwotab,margin=1)
        
        if (flip) {
          tttp <- tttp[c(2,1),]
          twobytwotab <- twobytwotab[c(2,1),]
        }
        
        p1.hat <- tttp[1,1]
        p2.hat <- tttp[2,1]
        n1 <- rowSums(twobytwotab)[1]
        n2 <- rowSums(twobytwotab)[2]
        Estimator <- p1.hat-p2.hat
        SE <- sqrt(p1.hat*(1-p1.hat)/n1+p2.hat*(1-p2.hat)/n2)
        
        if (!is.null(p)) {
        statistic <- Estimator/SE
        p.value <- GetP(statistic,alternative)
        }
        
        interval <- GetCI2(Estimator,SE,conf.level,alternative)
        
        successes <- twobytwotab[,1]
        sampsizes <- c(n1,n2)
        SummTab <- cbind(successes,
                         sampsizes,
                         props=successes/sampsizes)
        colnames(SummTab) <- c(success,"n","estimated.prop")
        rownames(SummTab) <- rownames(TwoWayTable)
        if (flip) rownames(SummTab) <- rev(rownames(TwoWayTable))
        
        results <- list(SummTab=SummTab,statistic=statistic,p.value=p.value,interval=interval,graph=graph,
                        explanatory=expname,response=respname,se=SE,conf.level=conf.level,
                        alternative=alternative,verbose=verbose)
        class(results)  <- "gcp2test"
        return(results)
      }
      
      if(length(pullout)==1)  {
        varname <- pullout[1]
        variable <- data[,varname]
        TallyTable <- xtabs(~variable)
        
        if (!(success %in% unique(variable))) {
          stop("No sucesses found.  Did you fail to specify success correctly?")
        }
        successes <- TallyTable[success]
        
        
        trials <- sum(TallyTable)
        
        Estimator <- successes/trials
        SE <- sqrt(Estimator*(1-Estimator)/trials)
        
        if(!is.null(p)) {
          
        if (correct==TRUE) {
          if (alternative=="less") statistic <- (Estimator+0.5/trials-p)/SE
          if (alternative=="greater") statistic <- (Estimator-0.5/trials-p)/SE
          if (alternative=="two.sided") statistic <- (Estimator-sign(Estimator)*0.5/trials-p)/SE
        } else statistic <- (Estimator-p)/SE
       
        p.value <- GetP(statistic,alternative)
        
          }
        
        interval <- GetCI1(Estimator,SE,conf.level,alternative)
        
        SummTab <- cbind(successes,
                         trials,
                         Estimator)
        colnames(SummTab) <- c(success,"n","estimated.prop")
        rownames(SummTab) <- ""
        
        results <- list(SummTab=SummTab,statistic=statistic,p.value=p.value,interval=interval,graph=graph,
                        variable=varname,se=SE,conf.level=conf.level,
                        alternative=alternative,correct=correct,p=p,verbose=verbose)
        class(results)  <- "gcp1test"
        return(results)
      }
    } #end form function
    
    
    if (is(x,"formula"))  {
      return(proptestGC.form(x,data,alternative=alternative,
                              conf.level=conf.level,
                              success=success,
                              correct=correct))
    }
    
    if (!is(x,"formula")) {  #we have summary data
      if (length(n)!=length(x))  {
        stop("Vector of counts and vector of trials must have same length")
      } 
      
      if (length(x)==1)  {  # one-sample
        successes <- x
        trials <- n
        
        Estimator <- successes/trials
        SE <- sqrt(Estimator*(1-Estimator)/trials)
        
        if(!is.null(p)) {
          
        if (correct==TRUE) {
          if (alternative=="less") statistic <- (Estimator+0.5/trials-p)/SE
          if (alternative=="greater") statistic <- (Estimator-0.5/trials-p)/SE
          if (alternative=="two.sided") statistic <- (Estimator-sign(Estimator)*0.5/trials-p)/SE
        } else statistic <- (Estimator-p)/SE
        
        p.value <- GetP(statistic,alternative)
        
        }
        
        interval <- GetCI1(Estimator,SE,conf.level,alternative)
        
        SummTab <- cbind(successes,
                         trials,
                         Estimator)
        colnames(SummTab) <- c("successes","n","estimated.prop")
        rownames(SummTab) <- ""
        
        results <- list(SummTab=SummTab,statistic=statistic,p.value=p.value,interval=interval,graph=graph,
                        variable=NA,se=SE,conf.level=conf.level,
                        alternative=alternative,correct=correct,p=p,verbose=verbose)
        class(results)  <- "gcp1test"
        return(results)
      }
      
      if (length(x)==2) {
        n1 <- n[1]
        n2 <- n[2]
        p1.hat <- x[1]/n1
        p2.hat <- x[2]/n2
        
        Estimator <- p1.hat-p2.hat
        SE <- sqrt(p1.hat*(1-p1.hat)/n1+p2.hat*(1-p2.hat)/n2)
        
        if(!is.null(p)) {
        statistic <- Estimator/SE  
        p.value <- GetP(statistic,alternative)
        }
        
        
        interval <- GetCI2(Estimator,SE,conf.level,alternative)
        
        SummTab <- cbind(x,
                         n,
                         x/n)
        colnames(SummTab) <- c("successes","n","estimated.prop")
        rownames(SummTab) <- c("Group 1","Group 2")
        
        results <- list(SummTab=SummTab,statistic=statistic,p.value=p.value,interval=interval,graph=graph,
                        explanatory=NA,response=NA,se=SE,conf.level=conf.level,
                        alternative=alternative,verbose=verbose)
        class(results)  <- "gcp2test"
        return(results)
      }
      
    }#end summary data treatment
    
  }#end proptestGC