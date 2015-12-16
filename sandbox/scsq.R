scsq <- function(x,data=parent.frame(),p=NULL) {
  
  prsd <- ParseFormula(x)
  pullout <- as.character(prsd$rhs)
  
  if (length(pullout) == 1) {  #one variable
    varname <- pullout[1]
    variable <- simpleFind(varName=varname,data=data)
    myTable <- xtabs(~variable) }
    nullsInput <- p
    
    # Define server logic for SlowGoodness
    server <- function(input, output) {
      
      
      simLimit <- 10000
      
      #Keep track of number of simulations in a given "set-up"
      numberSims <- 0
      chisqSims <- numeric()
      latestSim <- NULL
      fullSim <-character()
      
      #we also want the ability to refresh the "set-up
      total <- 0 #total number of sims over all set-ups including current one
      totalPrev <- 0 #total number of sims over all set-ups excluding current one
      
      
      obsInput <- as.vector(myTable)
      
      namesInput <- names(myTable)
      
      
      goodNulls <- reactive({
        nulls <- nullsInput
        goodNulls <- TRUE
        if (length(nulls) <= 1) goodNulls <- FALSE
        if (any(is.na(nulls))) goodNulls <- FALSE
        if (any(nulls <= 0)) goodNulls <- FALSE
        goodNulls     
      })
      
      goodObs <- reactive({
        obs <- obsInput
        goodObs <- TRUE
        if (length(obs) <= 1) goodObs <- FALSE
        if (any(is.na(obs))) goodObs <- FALSE
        if (any(obs < 0)) goodObs <- FALSE
        goodObs     
      })
      
      goodNames <- reactive({
        names <- namesInput
        goodNames <- TRUE
        if (length(names) <= 1) goodNames <- FALSE
        if (any(is.na(names))) goodNames <- FALSE
        goodNames     
      })
      
      obschisqInput <- reactive({
        nulls <- nullsInput/sum(nullsInput)
        totalCounts <- sum(obsInput)
        expected <- nulls*totalCounts
        sum(obsInput^2/expected)-totalCounts
      })
      
      simsUpdate <- reactive({
        if (input$resample > 0) {
          nullProbs <- isolate(nullsInput/sum(nullsInput))
          totalCounts <- isolate(sum(obsInput))
          expCounts <- nullProbs*totalCounts
          reps <- min(simLimit,isolate(input$sims))
          newSims <- rmultinom(n=reps,size=totalCounts,prob=nullProbs)
          chisqNew <- colSums(newSims^2/expCounts)-totalCounts
          chisqSims <<- c(chisqSims,chisqNew)
          latestSim <<- newSims[,reps]
          numberSims <<- numberSims + reps
          total <<- total+reps
          
          #now build fake list of outcomes for each trial, on the last sim
          varLevels <- isolate(namesInput)
          namesList <- rep(varLevels,times=latestSim)
          fullSim <<- sample(namesList,size=totalCounts,replace=FALSE)
          list(numberSims,latestSim)
        }
      })
      
      
      #this erases the simulation history and puts user back to initial graph
      simsReset <- reactive({
        input$reset
        totalPrev <<- totalPrev + numberSims
        numberSims <<- 0
        chisqSims <<- numeric()
        latestSim <<- NULL
        return(totalPrev)
      })
      
      
      dfInput <- reactive({
        length(obsInput)-1
      })
      
      
      xmaxInput <- reactive({
        qchisq(0.999,df=dfInput())
      })
      
      
      #help with conditonal panals
      output$totalPrev <- reactive({
        simsReset()
      })
      
      # needed for the conditional panels to work
      outputOptions(output, 'totalPrev', suspendWhenHidden=FALSE)
      
      output$total <- reactive({
        simsUpdate() #for dependency
        total
      })
      
      # needed for the conditional panels to work
      outputOptions(output, 'total', suspendWhenHidden=FALSE)
      
      output$barGraphInitial <- renderPlot({
        validate(
          need(goodNulls(),"Enter at least two null probabilities.  They should all be positive numbers.")
        )
        validate(
          need(goodObs(),"Enter at least two counts.  All counts should be non-negative integers.")
        )
        validate(
          need(goodNames(),"Enter a name for each possible outcome being tallied.")
        )
        
        observed <- obsInput
        nulls <- nullsInput/sum(nullsInput)
        names <- namesInput
        
        lengthCheck <- (length(nulls) == length(observed)) && (length(observed)==length(names))
        
        validate(
          need(lengthCheck,
               "Make sure that you enter the same number of null probabilities, counts and names.")
        )
        
        observed <- obsInput
        expected <- nulls*sum(observed)
        tab <- rbind(observed,expected)
        rownames(tab) <-c("Observed","Expected")
        colnames(tab) <- names
        barplot(tab,beside=T,col=c("#ee7700","grey"),
                main="Bargraph of Observed and Expected Counts",xlab="",ylab="Counts",
                legend.text=TRUE)
      })
      
      output$inittable <- renderTable({
        input$tablein
      })  
      
      output$remarksInitial <- renderText({
        
        observed <- obsInput
        nulls <- nullsInput/sum(nullsInput)
        names <- namesInput
        
        allGood <- (goodNulls() && goodObs()) && goodNames()
        lengthCheck <- (length(nulls) == length(observed)) && (length(observed)==length(names))
        
        validate(
          need(allGood && lengthCheck,"")
        )
        
        chisq <- obschisqInput()
        rounded1 <- round(chisq,2)
        paste("Observed chi-square statistic =  ",as.character(rounded1),sep="")
      })
      
      output$obsTable <- renderTable({
        
        observed <- obsInput
        nulls <- nullsInput/sum(nullsInput)
        names <- namesInput
        
        allGood <- (goodNulls() && goodObs()) && goodNames()
        lengthCheck <- (length(nulls) == length(observed)) && (length(observed)==length(names))
        
        validate(
          need(allGood && lengthCheck,"")
        )
        
        expected <- nulls*sum(observed)
        contribs <- (observed-expected)^2/expected
        df <- data.frame(Levels=names,
                         Observed=observed,
                         Expected=round(expected,2),
                         cont=round(contribs,2)
        )
        names(df)[4] <- c("Contribution to Chi-Square")
        df
      })
      
      output$remarksLatest1 <- renderText({
        input$resample
        chisq <- obschisqInput()
        rounded1 <- round(chisq,2)
        rounded2 <- round(chisqSims[length(chisqSims)],2)
        paste("Observed chi-square statistic =  ",as.character(rounded1),
              ", Latest resampled chi-square = ",as.character(rounded2),sep="")
      })
      
      output$remarksLatest2 <- renderText({
        input$resample
        chisq <- obschisqInput()
        rounded1 <- round(chisq,2)
        rounded2 <- round(chisqSims[length(chisqSims)],2)
        paste("Observed chi-square statistic =  ",as.character(rounded1),
              ", Latest resampled chi-square = ",as.character(rounded2),sep="")
      })
      
      
      output$barGraphLatest <- renderPlot({
        input$resample
        if (length(chisqSims) > 0) {
          totalCounts <- isolate(sum(obsInput))
          nulls <- isolate(nullsInput/sum(nullsInput))
          expected <- totalCounts*nulls
          tab <- rbind(obsInput,expected,latestSim)
          rownames(tab) <-c("Observed","Expected","Resampled")
          colnames(tab) <- isolate(namesInput)
          barplot(tab,beside=T,col=c("#ee7700","grey","#3333ff"),
                  main="Bargraph of Observed, Expected, and Latest Resample",xlab="",
                  ylab="Counts",
                  legend.text=TRUE)
        }
        
      })
      
      output$densityplot <-
        renderPlot({
          input$resample
          if (length(chisqSims)==1) band <- 1 else band <- "nrd0"
          dchisq <- density(chisqSims,n=500,from=0,to=xmaxInput(),bw=band)
          plot(dchisq$x,dchisq$y,type="l",col="blue",
               xlab="Chi-Square Value",ylab="Estimated Density",
               main="Distribution of Resampled Chi-Square Statistics")
          rug(chisqSims)
          latest <- chisqSims[length(chisqSims)]
          points(latest,0,col="blue",pch=19)
          abline(v=isolate(obschisqInput()))
          
        })
      
      output$summary1 <- renderTable({
        input$resample
        obs <- isolate(obschisqInput())
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
        obs <- obschisqInput()
        paste0("The percentage in the table gives the approximate probability, based on our resamples so far, of getting a chi-square statistic of ",
               round(obs,2)," or more, if the probability of each outcome is as the Null probabilities state.",
               "  The more resamples you take the better this approximations will be!")
      })
      
      
      output$summary2 <- renderTable({
        input$resample
        obs <- isolate(obschisqInput())
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
        obs <- obschisqInput()
        paste0("The curve above approximates the true probability distribution of the chi-square statistic.",
               " It is based on our resamples so far.  The percentage in the table gives the approximate probability, based on our resamples so far, of getting a chi-square statistic of ",
               round(obs,2)," or more, if the probability of each outcome is as the Null probabilities state.",
               "  The more resamples you take the better these approximations will be!")
      })
      
      
      output$chisqCurve <- renderPlot({
        obs <- obschisqInput()
        degFreedom <- dfInput()
        chisqGraph(bound=obs,region="above",df=degFreedom,xlab="Chi-Square Values",
                   graph=TRUE)
        abline(v=obs)
      })
      
      output$remarksProb <- renderText({
        obs <- obschisqInput()
        paste0("The curve above approximates the true probability distribution of the chi-square statistic.",
               " The shaded area gives the approximate probability of getting a chi-square statistic of ",
               round(obs,2)," or more, if the probability of each outcome is as the Null probabilities state.")
      })
      
    } #end server
    
    ui <- shinyUI(pageWithSidebar(
      
      #  Application title
      headerPanel("Chi-Square Goodness-of-Fit Resampling"),
      
      # Sidebar
      sidebarPanel(
        helpText("One simulation means the machine will produce one table of",
                 "counts, using the Null probabilities.  How many simulations do",
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
      
      # Here comes the main panel
      
      mainPanel(
        
        conditionalPanel(
          condition="input.resample == 0 || output.totalPrev == output.total",
          plotOutput("barGraphInitial"),
          p(textOutput("remarksInitial")),
          tableOutput("obsTable")
        ),
        
        conditionalPanel(
          condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
          tabsetPanel(selected="Latest Simulation",
                      tabPanel("Latest Simulation",
                               plotOutput("barGraphLatest"),
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
                               p(textOutput("remarksProb"))
                      ),
                      id="MyPanel"
          )
        )
        
        
      )
      
    ))  #end ui
    
    shiny::shinyApp(ui = ui, server = server)
  
}