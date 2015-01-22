#' @title Chi-Square Simulation (Contingency Table)

#' @description Perform chi-square test for association, by simulation.  Enter either
#' formula-data input or a summary table.
#' 
#' @rdname chisqSimShiny
#' @usage chisqSimShiny(x, data = parent.frame())
#' @param x Could be a formula.  If so, it should be of the form ~var1+var2.
#' Otherwise either a table or matrix of summary data.
#' @param data dataframe supplying variables for formula x.  If variables in x ar enot found in the data,
#' then they will be searched for in the parent environment.
#' @return side effects
#' @note This is a locally-run Shiny app.  It may not work properly on some R Stduio Server set-ups,
#' especially on the CentoOS operating system.
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' \dontrun{
#' # from a data frame:
#' chisqSimShiny(~seat,data=m111survey)
#' 
#' # from a summary table:
#' DoesNotSmoke <- c(NeitherSmokes=1168,OneSmokes=1823,BothSmoke=1380)
#' Smokes <- c(188,416,400)
#' ChildParents <- rbind(DoesNotSmoke,Smokes)
#' chisqSimShiny(ChildParents)
#' }
chisqSimShiny <- 
  function (x,data=parent.frame()) 
  {
    
###########################################################
#  begin with utiltiy functions
##########################################################

# temporary, for development

simpleFind <- function(varName,data) {
  
  if (is.null(data)) {
    return(get(varName,inherits=T))
  }
  
  tryCatch({get(varName,envir=as.environment(data))},
           error=function(e) {
             # is data name on the search path?
             dataName <- deparse(substitute(data))
             
             # will throw the error if data is not on search path 
             get(dataName,inherits=T)  
             
             # otherwise, user probably intends that this particular variable
             # is outside the stated data, so look for it:
             get(varName,inherits=T)
           }
  )
  
}

# temporary, for development

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

# Modified from pchisqGC.R in package tigerstats
chisqGraph <- function(bound,region="above",df=NA,xlab="chi_square_statistic",graph=FALSE) {
  if (!is.numeric(bound)) stop("Specify a numerical boundary")
  if (bound < 0)  stop("The chi-square statistic must be at least 0")
  if (is.na(df)) stop("Specify the degrees of freedom using the argument df")
  if (!(region %in% c("below","above"))) stop("Specify either \"region=\"below\" or
                                              \"region=\"above\"")
  if (df < 0) stop("Degrees of freedom must be positive")
  
  if (region=="below")  {
    area <- pchisq(bound,df=df)
    if (graph && df==1) warning("No graph produced for region below when df=1")
    if (graph) {
      bound <- round(bound,2)
      upper <- max(qchisq(.9999,df=df),bound+1)
      lower <- 0
      curve(dchisq(x,df=df),from=lower,to=upper,ylab="density",axes=FALSE,n=501,xlab=xlab,
            main=paste("Chi-Square Curve, df = ",df,"\nShaded Area = ",round(area,4)))
      axis(1,at=c(lower,bound,upper),labels=c(as.character(0),as.character(bound),""))
      axis(2)
      x.coords <- c(lower,seq(lower,bound,length.out=301),bound)
      y.coords <- c(0,dchisq(seq(lower,bound,length.out=301),df=df),0)
      polygon(x.coords,y.coords,col="lightblue",cex=2)
    }
  }
  
  if (region=="above")  {
    area <- pchisq(bound,df=df,lower.tail=FALSE)
    if (graph) {
      bound <- round(bound,2)
      upper <- max(qchisq(.9999,df=df),bound+1)
      lower <- 0
      curve(dchisq(x,df=df),from=lower,to=upper,ylab="density",axes=FALSE,n=501,xlab=xlab,
            main=paste("Chi-Square Curve, df = ",df,"\nShaded Area = ",round(area,4)))
      axis(1,at=c(lower,bound,upper),labels=c(as.character(0),as.character(bound),""))
      axis(2)
      x.coords <- c(bound,seq(bound,upper,length.out=301),upper)
      y.coords <- c(0,dchisq(seq(bound,upper,length.out=301),df=df),0)
      polygon(x.coords,y.coords,col="lightblue",cex=2)
    }
  }
  
  
  
}#end of chisqGraph
    

    exp.counts <- function(x) (rowSums(x) %*% t(colSums(x)))/sum(x)
    
    chisq.calc <- function(x) {
      expected <- exp.counts(x)
      contributions <- (x - expected)^2/expected
      return(sum(contributions[!is.nan(contributions)]))
    }
    
##########################################################################  
#    simulation for "rcfix" option
##########################################################################

    DoubleFixedResampler <- function(x,n) {
      expected <- exp.counts(x)
      csq <- function(x) {
        sum((x-expected)^2/expected)
      }
      statistic <- csq(x)
      nullDist <- numeric(n)
      
      r <- rowSums(x)
      c <- colSums(x)
      
      rtabs <- r2dtable(n,r=r,c=c)
      sims <- sapply(rtabs,FUN=csq,USE.NAMES=FALSE)

      return(list(sims=sims,last_table=rtabs[[n]]))
    }


#################################################   
#   simulation for "rfix" and "gtfix" options
#################################################

    RandFixedResampler <- function (x, n, effects = "random") 
    {
      #x is a two-way table, n is number of resamples
      TableResampler <- function(x, n = 1000, effects) {
        rowsampler <- function(x, p) {
          rmultinom(1, size = sum(x), prob = p)
        }
        
      table.samp <- function(x) {
        nullprobs <- colSums(x)/sum(x)
        resamp <- t(apply(x, 1, rowsampler, p = nullprobs))
        rownames(resamp) <- rownames(x)
        colnames(resamp) <- colnames(x)
        as.table(resamp)
      }
      
      rtabsamp <- function(x, n) {
          expected <- exp.counts(x)
          probs <- expected/sum(x)
          resamp.tab <- rmultinom(1, size = n, prob = probs)
          resamp.tab <- matrix(resamp.tab, nrow = nrow(x))
          rownames(resamp.tab) <- rownames(x)
          colnames(resamp.tab) <- colnames(x)
          return(resamp.tab)
      }
      
      resampled.tabs <- array(0, dim = c(nrow(x), ncol(x),n))
      
        if (effects == "fixed") {
          for (i in 1:n) {
            resampled.tabs[, , i] <- table.samp(x)
          }
          return(resampled.tabs)
        }
        if (effects == "random") {
          for (i in 1:n) {
            resampled.tabs[, , i] <- rtabsamp(x, sum(x))
          }
          return(resampled.tabs)
        }
      }
      tables <- TableResampler(x, n, effects = effects)
      nullDist <- apply(tables, 3, chisq.calc)
      return(list(sims=nullDist,last_table=tables[,,n]))
    }#end of RandFixedResampler
    

 
 ##################################################################
 ####
 ####
 #### Process Input
 ####
 ####
 #################################################################
    
  #first see if we have formula-data input, or summary data
  if (is(x,"formula")) #we have formula-data input
    {
    prsd <- ParseFormula(x)
    pullout <- as.character(prsd$rhs)
    
    if (length(pullout) != 3) {
      stop("Formula should have the form ~ var1 + var2")
    }
  
      
      expname <- as.character(prsd$rhs)[2]
      respname <- as.character(prsd$rhs)[3]
      
      explanatory <- simpleFind(varName=expname,data=data)
      response <- simpleFind(varName=respname,data=data)
      data2 <- data.frame(explanatory,response)
      names(data2) <- c(expname,respname)
      tab <- table(data2)
      
    } #end processing of formula
    
    
  if (!is(x,"formula"))  #we have summary data
  {
    if (length(dim(x)) !=2) #array more than two dimensions
    {
      stop("This function works only with two-dimensional contingency tables.")
    }
    
    tab <- as.table(x)
    
  }#end processing for summary data


# Define server logic for SlowGoodness
server <- function(input, output) {
   
  simLimit <- 10000 # no more than this many sims at one time
  
  #Keep track of number of simulations in a given "set-up"
  numberSims <- 0
  chisqSims <- numeric()
  latest_table <- NULL
  
  #we also want the ability to refresh the set-up
  total <- 0 #total number of sims over all set-ups including current one
  totalPrev <- 0 #total number of sims over all set-ups excluding current one
  
  # The observed two-way table:
  observed <- tab
  rowNames <- rownames(tab)
  colNames <- colnames(tab)
  
  expected <- exp.counts(tab)
  
  obschisq <- chisq.calc(observed)
  
  simsUpdate <- reactive({
    if (input$resample > 0) {
      reps <- min(simLimit,isolate(input$sims))
      simType <- isolate(input$simType) #probably don't need isolate
      if (simType=="rcfix") newSims <- DoubleFixedResampler(observed,reps)
      if (simType=="rfix") newSims <- RandFixedResampler(observed,reps,effects="fixed")
      if (simType=="gtfix") newSims <- RandFixedResampler(observed,reps,effects="random")
      chisqNew <- newSims$sims
      latest_table <<- newSims$last_table
      chisqSims <<- c(chisqSims,chisqNew)
      latestSim <<- newSims[reps]
      numberSims <<- numberSims + reps
      total <<- total+reps
      list(numberSims,latestSim)
    }
  })
  
  
  #this erases the simulation history and puts user back to initial graph
  simsReset <- reactive({
    input$reset
    totalPrev <<- totalPrev + numberSims
    numberSims <<- 0
    chisqSims <<- numeric()
    latest_table <<- NULL
    return(totalPrev)
  })
  
  
  df <- (nrow(observed)-1)*(ncol(observed)-1)
  
  xmax <- qchisq(0.999,df=df)
  
  #help with conditonal panels
  output$totalPrev <- reactive({
    simsReset()
  })
  
  # needed for the conditional panels to work
  outputOptions(output, 'totalPrev', suspendWhenHidden=FALSE)
  
  output$total <- reactive({
    simsUpdate() #for dependency
    total
  })
  
  # also needed for the conditional panels to work
  outputOptions(output, 'total', suspendWhenHidden=FALSE)
  
  
  output$remarksInitial <- renderText({
    paste("Observed chi-square statistic =  ",as.character(round(obschisq,2)),sep="")
  })
  
  output$obsTable <- renderTable({
    observed
  })

  output$expTable <- renderTable({
    expected <- exp.counts(observed)
    rownames(expected) <- rowNames
    expected
  })
  
  output$mosaicInitial <- renderPlot({
    par(mfrow=c(1,2))
    mosaicplot(t(observed),col="orange",main="Observed Table")
    expected <- exp.counts(observed)
    rownames(expected) <- rowNames
    mosaicplot(t(expected),col="grey",main="Expected Table")
    par(mfrow=c(1,1))
  })

  output$contrTable <- renderTable({
    (observed-exp.counts(observed))^2/exp.counts(observed)
  })
  
  output$remarksLatest1 <- renderText({
    input$resample
    rounded1 <- round(obschisq,2)
    rounded2 <- round(chisqSims[length(chisqSims)],2)
    paste("Observed chi-square statistic =  ",as.character(rounded1),
          ", Latest resampled chi-square = ",as.character(rounded2),sep="")
  })
  
  output$mosaicLatest <- renderPlot({
    if(input$resample > 0) { # for the dependency
      par(mfrow=c(1,2))
      rownames(latest_table) <- rowNames
      colnames(latest_table) <- colNames
      latest_table <- as.matrix(latest_table)
      mosaicplot(t(latest_table),col="blue",main="Simulated Table")
      expected <- exp.counts(latest_table)
      rownames(expected) <- rowNames
      mosaicplot(t(expected),col="grey",main="Expected Table\n(from simulation)")
      par(mfrow=c(1,1))
    }
  })

output$latestTable <- renderTable({
  input$resample
  if (!is.null(latest_table)) {
    rownames(latest_table) <- rowNames
    colnames(latest_table) <- colNames
    storage.mode(latest_table) <- "integer"
    return(latest_table)
  }
})
  
  output$remarksLatest2 <- renderText({
    input$resample
    rounded1 <- round(obschisq,2)
    rounded2 <- round(chisqSims[length(chisqSims)],2)
    paste("Observed chi-square statistic =  ",as.character(rounded1),
          ", Latest resampled chi-square = ",as.character(rounded2),sep="")
  })

  chisqDensities <- reactive({
    input$resample
    if (length(chisqSims)==1) band <- 1 else band <- "nrd0"
    density(chisqSims,n=500,from=0,to=xmax,bw=band)
  })
  
  output$densityplot <-
    renderPlot({
      input$resample
      dchisq <- chisqDensities()
      plot(dchisq$x,dchisq$y,type="l",col="blue",
           xlab="Chi-Square Value",ylab="Estimated Density",
           main="Distribution of Resampled Chi-Square Statistics")
      if (length(chisqSims) <= 200) rug(chisqSims)
      latest <- chisqSims[length(chisqSims)]
      points(latest,0,col="blue",pch=19)
      abline(v=isolate(obschisq))
      
    })
  
  output$summary1 <- renderTable({
    input$resample
    obs <- obschisq
    if (length(chisqSims) >0) {
      n <- length(chisqSims)
      latest <- chisqSims[n]
      p.value <- length(chisqSims[chisqSims>=obs])/n
      percentage <- paste(as.character(round(p.value*100,2)),"%",sep="")
      df <- data.frame(round(latest,2),n,percentage)
      names(df) <- c("Last Resampled Chi-Square",
                     "Number of Resamples So Far",
                     paste("Percent Above ",round(obs,2),sep="")
      )
      df  
    }
  })
  
  output$remarksProbBar <- renderText({
    obs <- obschisq
    paste0("The percentage in the table gives the approximate probability, based on our resamples so far, of getting a chi-square statistic of ",
           round(obs,2)," or more, if the Null Hypothesis (no relationship between the two factor",
           " variables under study) is true. The more resamples you take the better these",
           "approximations will be!")
  })
  
  
  output$summary2 <- renderTable({
    input$resample
    obs <- obschisq
    n <- length(chisqSims)
    latest <- chisqSims[n]
    p.value <- length(chisqSims[chisqSims>=obs])/n
    percentage <- paste(as.character(round(p.value*100,2)),"%",sep="")
    df <- data.frame(round(latest,2),n,percentage)
    names(df) <- c("Last Resampled Chi-Square",
                   "Number of Resamples So Far",
                   paste("Percent Above ",round(obs,2),sep="")
    )
    df    
  })
  
  output$remarksProbDensity <- renderText({
    obs <- obschisq
    paste0("The curve above approximates the true probability distribution of the chi-square statistic.",
           " It is based on our resamples so far.  The percentage in the table gives the approximate",
            "probability, based on our resamples so far, of getting a chi-square statistic of ",
           round(obs,2)," or more, if the Null Hypothesis (no relationship between the two factor",
           " variables under study) is true. The more resamples you take the better these",
            "approximations will be!")
  })
  
  
  output$chisqCurve <- renderPlot({
    obs <- obschisq
    degFreedom <- df
    chisqGraph(bound=obs,region="above",df=degFreedom,xlab="Chi-Square Values",
               graph=TRUE)
    abline(v=obs)
    if (input$compareDen) {
      lines(chisqDensities(),col="blue",lwd=4)
    }
  })
  
  output$remarksProb <- renderText({
    obs <- obschisq
    paste0("The curve above approximates the true probability distribution of the chi-square statistic.",
           " The shaded area gives the approximate probability of getting a chi-square statistic of ",
           round(obs,2)," or more, if the Null Hypothesis (no relationshoip between the two factor",
           " variables under study) is true.")
  })
  
} #end server

ui <- shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Chi-Square Goodness-of-Fit Resampling"),
  
  # Sidebar
  sidebarPanel(
    conditionalPanel(
      condition="input.resample == 0 || output.totalPrev == output.total",
      radioButtons(inputId="simType",
                    label="Choose type of simulation",
                    choices=c("Row and Column Sums Fixed"="rcfix",
                           "Row Sums Fixed"="rfix",
                           "Grand Total Fixed"="gtfix"))
      ),
    helpText("One simulation means the machine will produce one simulated table of",
             "counts, assuming the Null Hypothesis.  How many simulations do",
             "you want the machine to perform at once?  (Limit is 10000.)"),
    numericInput("sims","Number of Simulations at Once",1,min=0,step=1),
    br(),
    actionButton("resample","Simulate Now"),
    conditionalPanel(
      condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
      actionButton("reset","Start Over")
    )
  ),
  
  
  # Here comes the main panel
  
  mainPanel(
    
    conditionalPanel(
      condition="input.resample == 0 || output.totalPrev == output.total",
      plotOutput("mosaicInitial"),
      h4("Observed Table"),
      tableOutput("obsTable"),
      hr(),
      h4("Table Expected by the Null Hypothesis"),
       tableOutput("expTable"),
      hr(),
      h4("Contributions to the Chi-Square Statistic"),
      tableOutput("contrTable"),
      hr(),
      h5(textOutput("remarksInitial"))
    ),
    
    conditionalPanel(
      condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
      tabsetPanel(selected="Latest Simulation",
                  tabPanel("Latest Simulation",
                           plotOutput("mosaicLatest"),
                           h4("Last Simulated Table"),
                           tableOutput("latestTable"),
                           hr(),
                           p(textOutput("remarksLatest1")),
                           tableOutput("summary1"),
                           p(textOutput("remarksProbBar"))),
                  tabPanel("Density Plot of Simulations",
                           plotOutput("densityplot"),
                           p(textOutput("remarksLatest2")),
                           tableOutput("summary2"),
                           p(textOutput("remarksProbDensity"))),
                  tabPanel("Probability Distribution",
                           plotOutput("chisqCurve"),
                           hr(),
                           checkboxInput("compareDen","Compare with simulated chi-square distribution"),
                           p(textOutput("remarksProb"))
                  ),
                  tabPanel("Simulation Types",
                           includeHTML(system.file("doc/instructorNotes.html",
                                                   package="tigerstats"))),
                  id="MyPanel"
      )
    )
    
    
  )
  
))  #end ui


shiny::shinyApp(ui = ui, server = server)
    
    
  }#end chisqSimShiny

