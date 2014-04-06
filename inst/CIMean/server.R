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
#normalPop <- rnorm(10000,mean=muNorm,sd=sigmaNorm)
shapeGamma <- 2
scaleGamma <- 50
#skewPop <- rgamma(10000,shape=shapeGamma,scale=scaleGamma)

# for pareto:
alphaPareto <- 5
thetaPareto <- 100
tailProb <- 0.02  #want to find a Value at risk of 1 - this
valRisk <- thetaPareto*(tailProb^(-.5)-1)
superSkewPop <- rpareto(10000,alpha=alphaPareto,theta=thetaPareto)

# for pop with group of outliers
propOutliers <- 0.10
meanOutliers <- 200
sdOutliers <- 5
meanRegulars <- 50
sdRegulars <- 5
numbOutliers <- propOutliers*10000
numbRegulars <- (1-propOutliers)*10000
outlierPop <- c(rnorm(numbRegulars,mean=meanRegulars,sd=sdRegulars),
                rnorm(numbOutliers,mean=meanOutliers,sd=sdOutliers))

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
#normalDen <- density(normalPop,n=1024)

xSkew <- seq(0,shapeGamma*scaleGamma+7.5*sqrt(shapeGamma)*scaleGamma,
             length.out=600)
ySkew <- dgamma(xSkew,shape=shapeGamma,scale=scaleGamma)
skewDen <- list(x=xSkew,y=ySkew)
#skewDen <- density(skewPop,n=1024,from=0)

xSuperSkew <- seq(0,valRisk,length.out=600)
ySuperSkew <- dpareto(xSuperSkew,alpha=alphaPareto,theta=thetaPareto)
superSkewDen <- list(x=xSuperSkew,y=ySuperSkew)
#superSkewDen <- density(superSkewPop,n=1024,from=0)

xOut <- seq(0,meanOutliers+5*sdOutliers,length.out=600)
yOut <- (1-propOutliers)*dnorm(xOut,mean=meanRegulars,sd=sdRegulars)+propOutliers*dnorm(xOut,mean=meanOutliers,sd=sdOutliers)
outlierDen <- list(x=xOut,y=yOut)
#outlierDen <- density(outlierPop,n=1024)

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
  
  
  output$go <- reactive({
    input$go
  })
  
  output$actionType <- reactive({
    input$actionType
  })
  
  outputOptions(output, 'go', suspendWhenHidden=FALSE)
  outputOptions(output, 'actionType', suspendWhenHidden=FALSE)
  
  
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
  
  output$tstat <- renderPlot({
    frame <- intervalFrame()
    tstats <- frame$tstats
    n <- isolate(input$n)
    tstatDen <- density(tstats,n=1024,from=-6,to=6)
    ymax <- max(tstatDen$y,dt(0,df=n-1))
    plot(tstatDen$x,tstatDen$y,type="l",lwd=2,col="blue",
         main="t-statistic vs. t-curve",cex.main=2,
         xlab="t", ylim=c(0,ymax),
         ylab="density")
    curve(dt(x,df=n-1),-6,6,col="red",lwd=2,add=TRUE)
    
  })
  
  output$graphSample <- renderPlot({
    input$go
    popDen <- isolate(popDen())
    popMean <- isolate(popMean())
    n <- isolate(input$n)
    
    
    samp <- isolate(
      switch(input$popDist,
             normal=rnorm(n,mean=muNorm,sd=sigmaNorm),
             skew=rgamma(n,shape=shapeGamma,scale=scaleGamma),
             superskew=rpareto(n,alpha=alphaPareto,theta=thetaPareto),
             outliers=routlier(n)
        ))
    
    
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
    
  })
  
  intervalFrame <- reactive({
    input$go
    popMean <- isolate(popMean())
    n <- isolate(input$n)
    conf = isolate(input$confLevel/100)
    t.input = conf + ((1 - conf)/2)
    tMultiplier = qt(t.input, df = n - 1)
    
    action <- isolate(input$actionType)
    if (action == "fiveThousand") {
      itemNumb <- 5000*n
      sampleItems <- isolate(
        switch(input$popDist,
               normal=rnorm(itemNumb,mean=muNorm,sd=sigmaNorm),
               skew=rgamma(itemNumb,shape=shapeGamma,scale=scaleGamma),
               superskew=rpareto(itemNumb,alpha=alphaPareto,theta=thetaPareto),
               outliers=routlier(itemNumb)
        ))
      
      sampleMatrix <- matrix(sampleItems,ncol=n,nrow=5000)
      
      xbar <- rowSums(sampleMatrix)/n
      se <- sqrt((rowSums(sampleMatrix^2)-n*xbar^2)/(n^2-n))
      #se <- sqrt(diag(var(t(sampleMatrix))))/sqrt(n)
      margin = tMultiplier * se
      lower <- xbar - margin
      upper <- xbar + margin
      goodInterval <- ((popMean > lower) & (popMean < upper))
      goodInterval <- factor(ifelse(goodInterval,"yes","no"))
      
      tstats <- (xbar-popMean)/se
      
      results <- data.frame(
        tstats=tstats,
        LowerBound=lower,
        UpperBound=upper,
        PopulationMean=popMean,
        ContainsMean=goodInterval)
      results
    }
  })
  
  output$summary <- renderTable({
    
      frame <- intervalFrame()
      goodCount <- length(frame$ContainsMean[frame$ContainsMean=="yes"])
      tab <- data.frame("Good Intervals"=as.character(goodCount),
                        "Bad Intervals"=as.character(5000-goodCount),
                        "Estimated True Confidence Level"=paste((goodCount/50),"%",sep=""))
      tab
  })
  
  output$intervalFrame = renderDataTable({
      intervalFrame()[,2:5]}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5)
  )

  
})
  