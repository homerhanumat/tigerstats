library(shiny)

total <- 0
chisqstats <- numeric()

# Define server logic
shinyServer(function(input, output,session) {


  
  nullsInput <- reactive({
    probs <- as.numeric(unlist(strsplit(input$nulls,split=",")))
    probs/sum(probs)
    })
  
  obsInput <- reactive({
    observed <- as.integer(unlist(strsplit(input$obs,split=",")))
    observed 
  })
  
  namesInput <- reactive({
    unlist(strsplit(input$names,split=","))  
  })

  
  dfInput <- reactive({
    length(obsInput())-1
  })
  
  expectedInput <- reactive({
    nullsInput()*sum(obsInput())
  })
  
  resampInput <- reactive({ 
    input$resample
    size <- sum(obsInput())
    probs <- nullsInput()
    obsnew <- rmultinom(1,size=size,prob=probs)
    obsnew
  })
  
  obschisqInput <- reactive({
    expected <- nullsInput()*sum(obsInput())
    sum((obsInput()-expected)^2/expected)
  })
  
  obstabInput <- reactive({
    tab <- rbind(expectedInput(),obsInput())
    rownames(tab) <- c("Expected","Observed")
    colnames(tab) <- namesInput()
    tab
  })
  
  resamptabInput <- reactive({
    df <- data.frame(Levels=namesInput(),
                     Expected=round(expectedInput(),2),
                     Resampled=resampInput())
    t(df)
  })
  
  allchisqInput <- reactive({
    resetter()
    expected <- nullsInput()*sum(obsInput())
    new <- sum((resampInput()-expected)^2/expected)
    if (input$resample > total) chisqstats <<- c(chisqstats,new)
    chisqstats
  })
  
  newchisqInput <- reactive({
    n <- length(allchisqInput())
    allchisqInput()[n]
  })
  
  xmaxInput <- reactive({
    qchisq(0.999,df=dfInput())
  })
  
  resetter <- reactive({
    input$reset
    total <<- total+length(chisqstats)
    chisqstats <<- numeric()
  })
  
  output$resampcount <- reactive({
    resetter()
    length(allchisqInput())
  })
  
  outputOptions(output, 'resampcount', suspendWhenHidden=FALSE)

  output$chisqstats <- renderPrint({
    allchisqInput()
  })
  
  
  output$bargraphinit <- renderPlot({
    tab <- rbind(obsInput(),expectedInput())
    rownames(tab) <-c("Observed","Expected")
    colnames(tab) <- namesInput()
    barplot(tab,beside=T,col=c("#ee7700","grey"),
            main="Bargraph of Observed and Expected Counts",xlab="",ylab="Counts",
            legend.text=TRUE)
  })
  
  output$remark0 <- renderText({
    chisq <- obschisqInput()
    rounded1 <- round(chisq,2)
    paste("Observed chi-square statistic =  ",as.character(rounded1),sep="")
  })
  
  output$obstable <- renderTable({
    contribs <- (obsInput()-expectedInput())^2/expectedInput()
    df <- data.frame(Levels=namesInput(),
                     Observed=obsInput(),
                     Expected=round(expectedInput(),2),
                     cont=round(contribs,2)
                      )
    names(df)[4] <- c("Contribution to Chi-Square")
    df
  })
  
  output$chisqstat <- reactive({
    obschisqInput()
  })
  
  output$remark1 <- renderText({
    chisq <- obschisqInput()
    rounded1 <- round(chisq,2)
    rounded2 <- round(newchisqInput(),2)
    paste("Observed chi-square statistic =  ",as.character(rounded1),
          ", first resampled chi-square = ",as.character(rounded2),sep="")
  })
  
  output$remark2 <- renderText({
    chisq <- obschisqInput()
    rounded1 <- round(chisq,2)
    rounded2 <- round(newchisqInput(),2)
    paste("Observed chi-square statistic =  ",as.character(rounded1),
          ", last resampled chi-square = ",as.character(rounded2),sep="")
  })
  
  output$resampstats <- renderTable({
    resamps <- allchisqInput()
    DegFree <- dfInput()
    tab <- rbind(c(mean(resamps),sd(resamps)),
                 c(DegFree,sqrt(2*DegFree)))
    tab <- round(tab,2)
    rownames(tab) <- c("Resamples","Theoretical Chi-Square")
    colnames(tab) <- c("mean","standard deviation")
    tab
  })
  
  output$bargraph1 <- renderPlot({
    tab <- rbind(obsInput(),expectedInput(),t(resampInput()))
    rownames(tab) <-c("Observed","Expected","Resampled")
    colnames(tab) <- namesInput()
    barplot(tab,beside=T,col=c("#ee7700","grey","#3333ff"),
            main="Bargraph of Observed, Expected, and First Resample",xlab="",
            ylab="Counts",
            legend.text=TRUE)
    
  })
  
  output$bargraph <- renderPlot({
    tab <- rbind(obsInput(),expectedInput(),t(resampInput()))
    rownames(tab) <-c("Observed","Expected","Resampled")
    colnames(tab) <- namesInput()
    barplot(tab,beside=T,col=c("#ee7700","grey","#3333ff"),
            main="Bargraph of Observed, Expected, and Latest Resample",xlab="",
            ylab="Counts",
            legend.text=TRUE)
    
  })
  
  output$constructden <-
    renderPlot({
    resamps <- allchisqInput()
    dchisq <- density(resamps,n=500,from=0,to=xmaxInput())
    plot(dchisq$x,dchisq$y,type="l",col="blue",
         xlab="chi-square value",ylab="Estimated Density",
         main="Distribution of Resampled Chi-Square Statistics")
    rug(resamps)
    latest <- resamps[length(resamps)]
    points(latest,0,col="blue",pch=19)
    abline(v=obschisqInput())
    
    })
    
  output$tableden <- renderTable({
    resamptabInput()
  })
  
  
  output$summaryden <- renderTable({
    resamps <- allchisqInput()
    n <- length(resamps)
    latest <- resamps[n]
    p.value <- length(resamps[resamps>=obschisqInput()])/n
    percentage <- paste(as.character(round(p.value*100,2)),"%",sep="")
    df <- data.frame(round(latest,2),n,percentage)
    names(df) <- c("Last Resampled Chi-Square",
                   "Number of Resamples So Far",
                   paste("Percent Above ",round(obschisqInput(),2),sep="")
                  )
    df    
  })
  
  
})
  
