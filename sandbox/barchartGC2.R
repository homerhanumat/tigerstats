#' @title Easy Barcharts

#' @description Wrapper for \code{\link{barchart}} in package \code{lattice}.  Creates a 
#' barchart from raw data using formula-data syntax similar to that of \code{\link{xtabs}},
#' or from table.  Defaults to a "standard"
#' barchart in which the bars are vertical and unstacked.  Supports percentage barcharts.
#' 
#' @rdname barchartGC
#' @usage barchartGC(x,data=parent.frame(),type="frequency",auto.key=TRUE,
#'                        horizontal=FALSE,stack=FALSE,...)
#' @param x Either a formula or an object that can be coerced to a table.  If formula, it must be 
#' of the form ~var or ~var1+var2.
#' @param data Usually a data frame that supplies the variables in \code{x}.  Variables not in the data
#' argument are searched for in the parent environment.
#' @param type Possible values are "frequency" and "percent".
#' @param auto.key Provides a simple key
#' @param horizontal Determines orientation of the bars
#' @param stacked Determines whether bars for tallies are stacked on eac other or placed
#' next to one another
#' @param ... other arguments passed to \code{barchart}:  these include main, sub, and
#' xlab, which are likely to be familiar to students from other \code{lattice} graphical
#' functions.  An error is possible if other arguments
#' pertaining to legends are passed (hopefully anyone interested in such will have moved on
#' to \code{barchart}).
#' @return A trellis object describing the barchart.
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' #barchart of counts for one factor variable:
#' barchartGC(~sex,data=m111survey)
#' 
#' #barchart with percentages and title:
#' barchartGC(~sex,data=m111survey,
#'    type="percent",
#'    main="Distribution of Sex")
#' 
#' #barchart of counts, to study the relationship between
#' #two factor variables:
#' barchartGC(~sex+seat,data=m111survey)
#' 
#' #percentage barchart, two factor variables:
#' barchartGC(~sex+seat,data=m111survey,type="percent")
#' 
#' #From tabulated data:
#' sexseat <- xtabs(~sex+seat,data=m111survey)
#' barchartGC(sexseat,type="percent",main="Sex and Seating Preference")
#' 
#' #from tabulated data:
#' dieTosses <- c(one=8,two=18,three=11,four=7,five=9,six=7)
#' barchartGC(dieTosses,main="60 Rolls of a Die")
barchartGC <-
  function(x,data=parent.frame(),
           type="frequency",auto.key=TRUE,horizontal=FALSE,stack=FALSE,...)  { 
    
    ellipses <- list(...)
    
    if (type=="frequency") {
      if (is.null(ellipses$ylab)) {
        ellipses$ylab <- "Freq"
      }
    }
      
      if (type=="percent") {
        if (is.null(ellipses$ylab)) {
          ellipses$ylab <- "percentage"
        }
      }
        
    otherArgs <- c(list(stack=stack,horizontal=horizontal,
                        auto.key=auto.key),
              ellipses)
    
    if (is(x,"formula"))  { #we have a formula
      prsd <- ParseFormula(x)
      pullout <- as.character(prsd$rhs)
      
      if (length(pullout) == 1) {  #one variable
        varname <- pullout[1]
        variable <- simpleFind(varName=varname,data=data)
        tab <- xtabs(~variable) 
        if (type=="frequency") {
        args <- c(list(x=tab),otherArgs)
        return(do.call(barchart,args))
        }
      
        if (type=="percent") {
        perctab <- 100*tab/sum(tab)
        args <- c(list(x=perctab),otherArgs)
        return(do.call(barchart,args))
          }
      
      } # end one variable
      
      if (length(pullout)==3) { #two variables
        expname <- pullout[2]
        respname <- pullout[3]
        explanatory <- simpleFind(varName=expname,data=data)
        response <- simpleFind(varName=respname,data=data)
        tab <- table(explanatory,response)
        if (type=="frequency") {
        args <- c(list(x=tab),otherArgs)
        return(do.call(barchart,args))
        }
        if (type=="percent") {
          perctab <- 100*prop.table(tab,margin=1)
          args <- c(list(x=perctab),otherArgs)
          return(do.call(barchart,args))
        }   
      }
      
      
    } #end check for formula
      
    
    if (!is(x,"formula")) {  #we have tabular data
      x <- as.table(x)
      if (length(dim(x))==1) {#one variable
        if (type=="frequency") {
          args <- c(list(x=x),otherArgs)
          return(do.call(barchart,args))
        }
        if (type=="percent") {
          perctab <- 100*x/sum(x)
          args <- c(list(x=perctab),otherArgs)
          return(do.call(barchart,args))
        }     
      }
      if (length(dim(x))>1) {#two variables
        if (type=="frequency") {
          args <- c(list(x=x),otherArgs)
          return(do.call(barchart,args))
        }
        if (type=="percent") {
          perctab <- 100*prop.table(x,margin=1)
          args <- c(list(x=perctab),otherArgs)
          return(do.call(barchart,args))
      }
      
      }
        
    } # end tabular processing
    
  } #end barchartGC