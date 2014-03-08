#' @title t-Procedures

#' @description t-tests and confidence intervals for one and two samples.
#' 
#' @rdname ttestGC
#' @usage ttestGC(x=NULL,mean=numeric(),sd=numeric(),n=numeric(),
#'  mu=NULL,data=NULL,alternative="two.sided",var.equal=FALSE,
#'  conf.level=0.95,graph=FALSE,first=NULL,verbose=TRUE)
#' @param x If not NULL, then must be a formula.  If a formula, then data must be a dataframe.
#' For one sample t-procedures, x is of the form ~var.  For two-sample procedures,
#' x is of the form resp~exp, where exp is factor with two values.  If x is of form ~var1-var2,
#' then matched pairs procedures are performed .
#' @param mean When not NULL, contains sample mean(s).  Length 1 for one sample t-procedures,
#' Length 2 for two-sample procedures.
#' @param sd When not NULL, contains sample standard deviation(s).
#' @param n When not NULL, contains sample size(s).
#' @param mu Contains the null value for the parameter of interest.  If not set, no test is performed.
#' @param data A data frame containing variables in formula x.  Required when x is assigned..
#' @param alternative "two.sided" requests computation of a two-sided P-value;  other possible values are "less" and "greater".
#' @param var.equal When FALSE, use Welch's approximation to the degrees of freedom.
#' @param conf.level Number between 0 and 1 indicating the confidence-level of the interval supplied.
#' @param graph If TRUE, plot graph of P-value.
#' @param first  If assigned, gives the value of the explanatory variable that is to count
#' as the first sample.
#' @param verbose Indicate how much output goes to console
#' @return A list of class "GCttest" Components of the list that may be usefully 
#' queried include:  "statistic", "p.value", and "interval".
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' for matched pairs.
#' @examples
#' #One-sample t, 95%-confidence interval only:
#' ttestGC(~fastest,data=m111survey)
#' 
#' #For other confidence levels, set argument conf.level as desired.  For 90%-interval:
#' ttestGC(~fastest,data=m111survey,conf.level=0.90)
#' 
#' # One-sample t, 95%-confidence interval and two-sided test with H_0:  mu = 100:
#' ttestGC(~fastest,data=m111survey,mu=100)
#' 
#' #Two-sample t, 95%-confidence interval only:
#' ttestGC(fastest~sex,data=m111survey)
#' 
#' #control order of groups with argument first:
#' ttestGC(fastest~sex,data=m111survey,first="male")
#' 
#' # Matched pairs, confidence interval with one-sided test, H_0: mu-d = 0:
#' ttestGC(~ideal_ht-height,data=m111survey,alternative="less")
#' 
#' #Summary data, one sample, one-sided test with H_0:  mu = 52.5:
#' ttestGC(mean=55,sd=4,n=16,mu=52.5,alternative="greater")
#' 
#' #Summary data, two samples:
#' ttestGC(mean=c(50,55),sd=c(3,4),n=c(25,40),mu=0)
ttestGC <-
  function(x=NULL,mean=numeric(),sd=numeric(),n=numeric(),
           mu=NULL,data=NULL,alternative="two.sided", var.equal=FALSE,
           conf.level=0.95,graph=FALSE,first=NULL,verbose=TRUE)  {
    
    stat <- FALSE
    p.value <- FALSE  #These will get numerical values if a test is performed
    
    #Small Utility Function for p-values
    GetP <- function(stat,alternative,df) {
      switch(alternative,
             less=pt(stat,df=df),
             greater=pt(stat,df=df,lower.tail=FALSE),
             two.sided=2*pt(abs(stat),df=df,lower.tail=FALSE))
    }
    
    #small utility function for confidence intervals
    GetCI <- function(est,se,df,conf.level,alternative) {
      switch(alternative,
             less=c(lower=-Inf,upper=est+qt(conf.level,df=df)*se),
             two.sided=c(lower=est+qt((1-conf.level)/2,df=df)*se,upper=est+qt((1-conf.level)/2,df=df,lower.tail=FALSE)*se),
             greater=c(lower=est+qt(1-conf.level,df=df)*se,upper=Inf)
      )   
    }
    
    #small utility function for Welch's df approximation
    GetWdf <- function(s1,s2,n1,n2){
        df <- (s1^2/n1+s2^2/n2)^2/(s1^4/(n1^2*(n1-1))+s2^4/(n2^2*(n2-1)))
      return(round(df,2))
    }
    
    #Methods for formulas come next.
    #First, one sample formula ~var:
    t.test.f1 <- function(x,
                          mu=mu,data,alternative,
                          conf.level,graph)  {
      
      varname <- as.character(ParseFormula(x)$rhs)
      if (!(varname %in% names(data))) {
        stop(paste(varname,"is not a variable in",data))
      }
      var <- data[,varname]
      n <- length(var[!is.na(var)])
      xbar <- mean(var,na.rm=T)
      stdev <- sd(var,na.rm=T)
      sterr <- stdev/sqrt(n)
      
      if (!is.null(mu)) {
      stat <- (xbar-mu)/sterr
      p.value <- GetP(stat,alternative,df=n-1)
      }
      
      interval <- GetCI(est=xbar,se=sterr,
                        conf.level=conf.level,alternative=alternative,df=n-1)
      summtab <- data.frame(variable=varname,
                           mean=xbar,sd=stdev,n=n)
      results <- list(SummTab=summtab,
                      estimate=xbar,
                      se=sterr,
                      statistic=stat,p.value=p.value,
                      interval=interval,
                      alternative=alternative,mu=mu,
                      conf.level=conf.level,
                      graph=graph,df=n-1,subm="f1",varnames=varname,
                      verbose=verbose)
      #subm helps print function format results
      return(results)
    }
    
    t.test.f2 <-  function(x,
                           mu,data,alternative,
                           conf.level,graph,first) {
      prsd <- ParseFormula(x)
      respname <- as.character(prsd$lhs)
      expname <- as.character(prsd$rhs)
      if (!(respname %in% names(data))) {
        stop(paste(respname,"is not a variable in",data))
      }
      if (!(expname %in% names(data))) {
        stop(paste(expname,"is not a variable in",data))
      }
      
      
      resp <- data[,respname]
      exp <- data[,expname]
      
      if (length(unique(exp))!=2) {
        stop("Explanatory variable must have exactly two values;",expname,
             "has",length(unique(exp)),"values.")
      }
      ordergroups <- !is.null(first)
      
      if(ordergroups && !(first %in% unique(exp))) {
        stop(paste(first,"is not a value of",expname))
      }
      
      #now cut out missing values
      data <- data[,c(expname,respname)]
      data <- data[complete.cases(data),]
      exp <- data[,expname]
      resp <- data[,respname]
      
      if (length(unique(exp)) < 2) {
        print(table(exp))
        stop("After excluding missing values, one of the groups is empty.")
      }
      
      nameorder<- sort(unique(exp))
      
      if (ordergroups) {
        temp <- nameorder
        nameorder[2] <- temp[which(temp!=first)]
        nameorder[1] <- first
      }
      
      sd1 <- sd(resp[exp==nameorder[1]])
      sd2 <- sd(resp[exp==nameorder[2]])
      xbar1 <- mean(resp[exp==nameorder[1]])
      xbar2 <- mean(resp[exp==nameorder[2]])
      n1 <- length(resp[exp==nameorder[1]])
      n2 <- length(resp[exp==nameorder[2]])
      
      sterr <- sqrt(sd1^2/n1+sd2^2/n2)
      est <- xbar1-xbar2
      
      
      if (var.equal==TRUE) {
        df <- n1+n2-2
        subm <- "f2e"
      }
      else {
        df <- GetWdf(s1=sd1,s2=sd2,n1=n1,n2=n2)
        subm <- "f2u"
      }
      if (!is.null(mu)) {
      stat <- (xbar1-xbar2-mu)/sterr
      p.value <- GetP(stat,alternative,df=df)
      }
      
      interval <- GetCI(est=xbar1-xbar2,se=sterr,
                        conf.level=conf.level,alternative=alternative,df=df)
      summtab <- data.frame(group=nameorder,mean=c(xbar1,xbar2),
                            sd=c(sd1,sd2),
                            n=c(n1,n2)
      )
      
      
      results <- list(SummTab=summtab,
                      estimate=xbar1-xbar2,
                      se=sterr,
                      statistic=stat,p.value=p.value,
                      interval=interval,
                      alternative=alternative,mu=mu,
                      conf.level=conf.level,
                      graph=graph,df=df,subm=subm,varnames=c(expname,respname),
                      verbose=verbose)
      return(results)
    }
    
    t.test.fm <-  function(x,
                           mu,data,alternative,
                           conf.level,graph,first) {
      prsd <- ParseFormula(x)
      var1name <- as.character(prsd$rhs)[2] #weirdly, the - comes first!
      var2name <- as.character(prsd$rhs)[3]
      if (!(var1name %in% names(data))) {
        stop(paste(var1name,"is not a variable in the dataframe."))
      }
      if (!(var2name %in% names(data))) {
        stop(paste(var2name,"is not a variable in the dataframe."))
      }
      
      #now cut out missing values
      data <- data[,c(var1name,var2name)]
      data <- data[complete.cases(data),]
      var1 <- data[,var1name]
      var2 <- data[,var2name]
      
      if (length(var1) < 2) {
        stop("Need two or more items with both measures recorded.")
      }
      
      diff <- var1-var2
          
      sd <- sd(diff)
      dbar <- mean(diff)
      n <- length(diff)
      
      sterr <- sd/sqrt(n)
      est <- dbar
     
      df <-  n-1
      
      if (!is.null(mu)) {
      stat <- (dbar-mu)/sterr
      p.value <- GetP(stat,alternative,df=df)
      }
      
      interval <- GetCI(est=est,se=sterr,
                        conf.level=conf.level,alternative=alternative,df=df)
      summtab <- data.frame(Difference=paste(var1name,"-",var2name),
                            mean.difference=dbar,
                            sd.difference=sd,
                            n=n)    
      
      results <- list(SummTab=summtab,
                      estimate=dbar,
                      se=sterr,
                      statistic=stat,p.value=p.value,
                      interval=interval,
                      alternative=alternative,mu=mu,
                      conf.level=conf.level,
                      graph=graph,df=df,subm="fm",varnames=c(var1name,var2name),
                      verbose=verbose)
      return(results)
    }
    
    t.test.s1 <- function(mean,sd,n,mu,conf.level,alternative,graph) {
      xbar <- mean
      stdev <- sd
      sterr <- stdev/sqrt(n)
      
      if (!is.null(mu)) {
      stat <- (xbar-mu)/sterr
      p.value <- GetP(stat,alternative,df=n-1)
      }
      
      interval <- GetCI(est=xbar,se=sterr,
                        conf.level=conf.level,alternative=alternative,df=n-1)
      summtab <- data.frame(mean=xbar,sd=stdev,n=n)
      results <- list(SummTab=summtab,
                      estimate=xbar,
                      se=sterr,
                      statistic=stat,p.value=p.value,
                      interval=interval,
                      alternative=alternative,mu=mu,
                      conf.level=conf.level,
                      graph=graph,df=n-1,subm="s1",varnames=NA,
                      verbose=verbose)
      return(results)
    }
    
    t.test.s2 <- function(mean,sd,n,mu,conf.level,alternative,graph) {
      sd1 <- sd[1]
      sd2 <- sd[2]
      xbar1 <- mean[1]
      xbar2 <- mean[2]
      n1 <- n[1]
      n2 <- n[2]
      
      sterr <- sqrt(sd1^2/n1+sd2^2/n2)
      est <- xbar1-xbar2
     
      
      if (var.equal==TRUE) {
        df <- n1+n2-2
        subm <- "s2e"
      }
      else {
        df <- GetWdf(s1=sd1,s2=sd2,n1=n1,n2=n2)
        subm <- "s2u"
      }
      
      if (!is.null(mu)) {
      stat <- (xbar1-xbar2-mu)/sterr
      p.value <- GetP(stat,alternative,df=df)
      }
      
      interval <- GetCI(est=xbar1-xbar2,se=sterr,
                        conf.level=conf.level,alternative=alternative,df=df)
      summtab <- data.frame(group=c("Group 1","Group 2"),mean=c(xbar1,xbar2),
                            sd=c(sd1,sd2),
                            n=c(n1,n2)
      )
      
      
      results <- list(SummTab=summtab,
                      estimate=xbar1-xbar2,
                      se=sterr,
                      statistic=stat,p.value=p.value,
                      interval=interval,
                      alternative=alternative,mu=mu,
                      conf.level=conf.level,
                      graph=graph,df=df,subm=subm,varnames=NA,
                      verbose=verbose)
      return(results)
    }
    
    #Finally, the main body of the function itself:
    
    #Go easy on values for arguments
    if (grepl("^g",alternative,perl=TRUE)) alternative <- "greater"
    if (grepl("^l",alternative,perl=TRUE)) alternative <- "less"
    if (grepl("^[tn]",alternative,perl=TRUE)) alternative <- "two.sided"
    
      #Are we looking at a formula?
      if (!is.null(x) && is(x,"formula")){
        prsd <- ParseFormula(x)
        if (is.null(prsd$lhs) && length(as.character(prsd$rhs))==1) {
          res <- t.test.f1(x=x,mu=mu,data=data,alternative=alternative,
                           conf.level=conf.level,graph=graph)
        }
        
        if (is.null(prsd$lhs) && length(as.character(prsd$rhs))==3) {
          res <- t.test.fm(x=x,mu=mu,data=data,alternative=alternative,
                           conf.level=conf.level,graph=graph)
        }
        
        if (!is.null(prsd$lhs)) {
          res <- t.test.f2(x=x,mu=mu,data=data,alternative=alternative,
                           conf.level=conf.level,graph=graph,first=first)
        }
        
      }
    
    #Next, the case of summarized data:
    if (is.null(x)) {
    if (length(mean)==1 && (length(sd)==1 && length(n)==1)) {
      res <- t.test.s1(mean=mean,sd=sd,n=n,alternative=alternative,mu=mu,
                       conf.level=conf.level,graph=graph)
    } else {
      if (length(mean)==2 && (length(sd)==2 && length(n)==2)) {
      res <- t.test.s2(mean=mean,sd=sd,n=n,alternative=alternative,mu=mu,
                       conf.level=conf.level,graph=graph)}
            else stop("Summary data entered incorrectly.")
    }
  }
    
    class(res) <- "GCttest"
    return(res)
    
    } #end ttestGC