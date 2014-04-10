library(shiny)





# Define server logic for FindRegLine
shinyServer(function(input, output) {
  
  
  turns <- -1
  
  lowa <- -5
  higha <- 5
  lowb <- -2
  highb <- 2
  sigma <- 3
  ta <- round(runif(1,min=lowa,max=higha),1)
  tb <- round(runif(1,min=lowb,max=highb),1)
  n <- 10  #number of points
  x <- 1:n
  y <- ta+tb*x+rnorm(n,mean=0,sd=sigma)
  
  #SS for the regression line:
  mod <- lm(y~x)
  ess <- sum((resid(mod))^2)
  
  #determine nice limits for plot (and a slider):
  reg.slope <- coef(mod)[2]
  reg.int <- coef(mod)[1]
  
  #Find range of y-intercepts of lines through
  #points on scatterplot having slope = reg.slope
  int.min <- min(y-reg.slope*x)
  int.max <- max(y-reg.slope*x)
  int.band <- (int.max-int.min)/2
  
  #Expand this range, and make sure it includes 0:
  int.mid <- (int.max+int.min)/2
  lowa.slider <- floor(min(c(int.mid-1.2*int.band,-1,min(y)-1)))
  higha.slider <- ceiling(max(c(int.mid+1.2*int.band,1,max(y)+1)))
  
  #plot limits reflect this range, too:
  ymin <- lowa.slider
  ymax <- higha.slider
  y.mean <- mean(y)
  
  
  #SS for the line initially placed (a=0,b=0):
  total.ss <- sum((y-mean(y))^2)
  your.ss <- total.ss
  #You start at line with slope 0, intercept = mean(y)  



  
  #make the a and b sliders
  output$aslider <- renderUI({
        sliderInput("a",min=lowa.slider,max=higha.slider,
                label="y-Intercept",step=0.01,value=y.mean)
        })
  
  output$bslider <- renderUI({
        sliderInput("b",min=2*lowb,max=2*highb,label="Slope",
                    step=0.01,value=0)
          })
  
  turnIncrement <- reactive({
    input$submit
    turns <<- turns+1
    turns
  })
  
  
 output$gamecloud <- renderPlot({
  input$a
  input$b
  plot(x,y,pch=16,col="blue",ylim=c(ymin,ymax),
       xlim=c(0,n))
  points(0,0,cex=0.8,pch=16,col="green")
  abline(input$a,input$b)
  abline(0,0,lty=2,col="green")
  lines(x=c(0,0),y=c(ymin,ymax),lty=2,col="green")
  your.y <- input$a+input$b*x
  for(i in 1:n)  {
    lines(x=c(x[i],x[i]),y=c(your.y[i],y[i]))
  }
 })
 
 output$finalcloud <- renderPlot({
   input$submit
   plot(x,y,pch=16,col="blue",ylim=c(ymin,ymax),
        xlim=c(0,n))
   points(0,0,cex=0.8,pch=16,col="green")
   isolate(abline(input$a,input$b))
   abline(0,0,lty=2,col="green")
   lines(x=c(0,0),y=c(ymin,ymax),lty=2,col="green")
   coefs <- coef(mod)
   abline(coefs,col="red",lwd=3)
 })
 
 output$score <- renderTable({
   turnIncrement()
   if (turns > 0) {
   your.y <- isolate(input$a+input$b*x)
   fits <- fitted(mod)
   your.ss <- sum((y-your.y)^2)
    }
   close <- 100*(your.ss-ess)/(total.ss-ess)
   score <- turns+close
   tab <- rbind(your.ss,ess,close,turns,round(score,3))
   colnames(tab) <- "Score Report"
   rownames(tab) <- c("Your Error Sum of Squares",
              "Regression Line's Error Sum of Squares",
              "Closeness Measure",
              "Turns So Far","Score (Turns + Closeness)")
   tab
 })
 
 output$revelation <- renderTable({
   coefs <- round(coef(mod),2)
   tab <- rbind(c(input$a,input$b),coefs)
   rownames(tab) <- c("Your Final Guesses","Regression Line")
   colnames(tab) <- c("y-Intercept","Slope")
   tab
 })
  
})

