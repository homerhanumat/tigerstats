library(shiny)

########################################
#  Utility Functions for Generating Pareto Values
########################################

rpareto <- function(n,alpha,theta) {#random values for Pareto(alpha,theta) distribution
  theta*((1-runif(n))^(-1/alpha)-1)
}

dpareto <- function(x,alpha,theta) {  #pdf for Pareto(alpha,theta) distribution
  alpha*theta^alpha/(x+theta)^(alpha+1)
}


#############################################
# Generate the populations
############################################
muNorm <- 70
sigmaNorm <- 5
shapeGamma <- 2
scaleGamma <- 50

# for pareto:
alphaPareto <- 5
thetaPareto <- 100
tailProb <- 0.02  #want to find a Value at risk of 1 - this
valRisk <- thetaPareto*(tailProb^(-.5)-1)

# for pop with group of outliers
propOutliers <- 0.10
meanOutliers <- 200
sdOutliers <- 5
meanRegulars <- 50
sdRegulars <- 5

routlier <- function(n) {
  propNormals <- 1- propOutliers
  whichHump <- rbinom(n,size=1,prob=propNormals)
  outlierSamp <- ifelse(whichHump,rnorm(n,mean=meanRegulars,sd=sdRegulars),
                        rnorm(n,mean=meanOutliers,sd=sdOutliers))
  outlierSamp
}

######################################
# Make population densities
#####################################
xNorm <- seq(muNorm-5*sigmaNorm,muNorm+5*sigmaNorm,length.out=600)
yNorm <- dnorm(xNorm,mean=muNorm,sd=sigmaNorm)
normalDen <- list(x=xNorm,y=yNorm)

xSkew <- seq(0,shapeGamma*scaleGamma+7.5*sqrt(shapeGamma)*scaleGamma,
             length.out=600)
ySkew <- dgamma(xSkew,shape=shapeGamma,scale=scaleGamma)
skewDen <- list(x=xSkew,y=ySkew)

xSuperSkew <- seq(0,valRisk,length.out=600)
ySuperSkew <- dpareto(xSuperSkew,alpha=alphaPareto,theta=thetaPareto)
superSkewDen <- list(x=xSuperSkew,y=ySuperSkew)

xOut <- seq(0,meanOutliers+5*sdOutliers,length.out=600)
yOut <- (1-propOutliers)*dnorm(xOut,mean=meanRegulars,sd=sdRegulars)+propOutliers*dnorm(xOut,mean=meanOutliers,sd=sdOutliers)
outlierDen <- list(x=xOut,y=yOut)

#######################################
# Get the population means
######################################

normalMean <- muNorm
skewMean <- shapeGamma*scaleGamma
superSkewMean <- thetaPareto/(alphaPareto - 1)
outlierMean <- (1-propOutliers)*meanRegulars+propOutliers*meanOutliers

# Define server logic
shinyServer(function(input, output) {
  
  popDen <- reactive({
    switch(input$popDist,
           normal=normalDen,
           skew=skewDen,
           superskew=superSkewDen,
           outliers=outlierDen)
  })
  
  popMean <- reactive({
    switch(input$popDist,
           normal=normalMean,
           skew=skewMean,
           superskew=superSkewMean,
           outliers=outlierMean)
  })
  
  popMax <- reactive({
    switch(input$popDist,
           normal=max(normalDen$x),
           skew=max(skewDen$x),
           superskew=max(superSkewDen$x),
           outliers=max(outlierDen$x)
    )
  })
  
  popMin <- reactive({
    switch(input$popDist,
           normal=min(normalDen$x),
           skew=min(skewDen$x),
           superskew=min(superSkewDen$x),
           outliers=min(outlierDen$y)
    )
  })
  
  yMax <- reactive({
    densities <- popDen()
    max(densities$y)*1.5
  })
  

  simLimit <- 10000 #upper limit on number of sims at once
  
  #Keep track of number of simulations in a given "set-up"
  numberSims <- 0
  intervalResults <- numeric()
  tstats <- numeric()
  latestSamp <- numeric()
  
  #we also want the ability to refresh the "set-up
  total <- 0 #total number of sims over all set-ups including current one
  totalPrev <- 0 #total number of sims over all set-ups excluding current one
  
  simsUpdate <- reactive({
    if (input$resample > 0) {


      reps <- min(simLimit,isolate(input$sims))
      total <<- total + reps
      numberSims <<- numberSims + reps
      
      popMean <- isolate(popMean())
      n <- isolate(input$n)
      conf = isolate(input$confLevel/100)
      t.input = conf + ((1 - conf)/2)
      tMultiplier = qt(t.input, df = n - 1)
      
      itemNumb <- reps*n
      sampleItems <- isolate(
        switch(input$popDist,
               normal=rnorm(itemNumb,mean=muNorm,sd=sigmaNorm),
               skew=rgamma(itemNumb,shape=shapeGamma,scale=scaleGamma),
               superskew=rpareto(itemNumb,alpha=alphaPareto,theta=thetaPareto),
               outliers=routlier(itemNumb)
        ))
      
      sampleMatrix <- matrix(sampleItems,ncol=n,nrow=reps)
      
      latestSamp <<- sampleMatrix[reps,]
      
      xbar <- rowSums(sampleMatrix)/n
      se <- sqrt((rowSums(sampleMatrix^2)-n*xbar^2)/(n^2-n))
      margin = tMultiplier * se
      lower <- xbar - margin
      upper <- xbar + margin
      goodInterval <- ((popMean > lower) & (popMean < upper))
      intervalResults <<- c(intervalResults,goodInterval)
      
      tstats <<- c(tstats,(xbar-popMean)/se)
      
    } #end if resample
      
  })
  
  #this erases the simulation history and puts user back to initial graph
  simsReset <- reactive({
    input$reset
    totalPrev <<- totalPrev + numberSims
    numberSims <<- 0
    intervalResults <<- numeric()
    tstats <<- numeric()
    latestSamp <<- numeric()
    return(totalPrev)
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
  
  
  output$initialGraph <- renderPlot({
    popDen <- popDen()
    popMean <- popMean()
    plot(popDen$x,popDen$y,type="l",lwd=3,col="red",
         main="Density Curve of Population",
         xlab="",
         ylab="density")
    abline(v=popMean,lwd=2)
    
  })
  
  output$initialGraph2 <- renderPlot({
    input$go
    popDen <- isolate(popDen())
    popMean <- isolate(popMean())
    plot(popDen$x,popDen$y,type="l",lwd=3,col="red",
         main="Density Curve of Population",
         xlab="",
         ylab="density")
    #now add mean line:
    abline(v=popMean,lwd=2)
    
  })
  
  output$tstatistic <- renderPlot({
    input$resample
    n <- isolate(input$n)
    if (numberSims == 1) {
      tstatDen <- density(tstats,n=1024,from=-10,to=10,bw=1)
    }
    if (numberSims >= 2 && n < 5) {
      tstatDen <- density(tstats,n=1024,from=-10,to=10,bw=0.1)
    }
    if (numberSims >= 2 && n >= 5) {
      tstatDen <- density(tstats,n=1024,from=-10,to=10,bw="SJ")
    }
    
    if (numberSims > 0) {
    ymax <- max(tstatDen$y,dt(0,df=n-1))
    plot(tstatDen$x,tstatDen$y,type="l",lwd=2,col="blue",
         main="t-statistic vs. t-curve",cex.main=2,
         xlab="t", ylim=c(0,ymax),xlim=c(-6,6),
         ylab="density")
    curve(dt(x,df=n-1),-6,6,col="red",lwd=2,add=TRUE)
    } #end check that there are samples
    
  })
  
  output$graphSample <- renderPlot({
    input$resample #for the dependency
    if(numberSims > 0) {
    popDen <- isolate(popDen())
    popMean <- isolate(popMean())
    n <- isolate(input$n)
    
    samp <- latestSamp
    
    
    xmin <- isolate(popMin())
    xmax <- isolate(popMax())
    ymax <- isolate(yMax())
    intLevel <- 0.95*ymax  #how high on plot to put conf interval
    
    conf = isolate(input$confLevel/100)
    xbar = mean(samp)
    t.input = conf + ((1 - conf)/2)
    tMultiplier = qt(t.input, df = n - 1)
    se = sd(samp)/sqrt(n)
    margin = tMultiplier * se
    ci = c(xbar - margin, xbar + margin)
    
    if ((ci[1] < popMean) & (ci[2] > popMean)) {
      result <- "contains the population mean!"
    }
    else {
      result <- "is a lemon."
    }
    
    hist(samp,freq=FALSE,col="lightblue",xlim=c(xmin,xmax),
         ylim=c(0,ymax),
         main="Population Density Curve\nand Histogram of Sample",
         xlab="",
         sub=paste("This confidence interval ",result,sep=""),
         cex.main=2,cex.sub=2
    )
    lines(popDen,lwd=2,col="red")
    abline(v=popMean,lwd=2)
    
    
    segments(x0 = ci[1], y0 = intLevel, x1 = ci[2], y1 = intLevel, 
             col = "green", lwd = 3)
    text(x=ci[1],y=intLevel,labels="(")
    text(x=ci[2],y=intLevel,labels=")")
    points(xbar, intLevel, col = "blue", pch = 20,cex=2)
    
    } # end checking that we have a latest sample
    
  })
  

  
  output$summary <- renderTable({
      input$resample #for the dependency
      goodCount <- sum(intervalResults)
      tab <- data.frame(
                "Good Intervals"=goodCount,
                "Total Intervals"=numberSims,
                "Estimated True Confidence Level"=paste(round(goodCount/numberSims*100,3),"%",sep="")
                )
      tab
  })

  
})
  