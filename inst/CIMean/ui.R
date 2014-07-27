library(shiny)




# Define UI for testing application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Confidence Intervals for a Population Mean"),
  
  # Sidebar
  sidebarPanel(
    conditionalPanel(
      condition="input.resample == 0 || output.totalPrev == output.total",
      selectInput(inputId="popDist",label="Population Shape",
                choices=list("Normal"="normal",
                             "Skewy"="skew",
                             "REALLY Skewed"="superskew",
                             "Way-Out Outlier Group"="outliers")),
      br(),
    
      helpText("Choose the sample size."),
    
      sliderInput(inputId="n","Sample Size n",value=2,min=2,max=50,step=1),
      br(),
    
      helpText("How confident do you want to be that the population mean is contained",
             "within the confidence interval?   Use the slider to select a desired",
             "percent-confidence level."),
    
      sliderInput(inputId="confLevel","Confidence Level",value=80,min=50,max=99,step=1)
        ),
      helpText("How many samples would you like to take at one time?  Limit is 10000. With each ",
             "sample, we'll make a confidence interval for the population mean."),
      numericInput("sims","Number of Samples at Once",1,min=0,step=1),
      actionButton("resample","Sample Now"),
      conditionalPanel(
        condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
        actionButton("reset","Start Over")
      )
    
  ),
  
  
  # Here comes the main panel
  
  mainPanel(
    
    conditionalPanel(
      condition="input.resample == 0 || output.totalPrev == output.total",
      plotOutput("initialGraph")
    ),

    
    conditionalPanel(
      condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
      
      tabsetPanel(
        tabPanel("Latest Interval",
                 plotOutput("graphSample"),
                 HTML("<p> </p>"),
                 HTML("<ul>
                        <li>The population density curve is in red.</li>
                        <li>The vertical line marks the population mean.</li>
                        <li>The histogram of the sample is in light blue.</li>
                        <li>The sample mean is the big blue dot.</li>
                        <li>The confidence interval is in green.</li>
                      </ul>")),
        tabPanel("Summary of Intervals",
                 plotOutput("initialGraph2"),
                 tableOutput("summary")), 
        tabPanel("t-statistic",
                 plotOutput("tstatistic"),
                 HTML(
                   "<p>The plots above compare the actual distribution of the t-statistic to the t-curve with n-1 degrees of freedom.</p>
                     <p></p>
                     <ul>
                        <li>The t-curve is in red.  If the population is exactly normal, then this curve represents the exact distribution of the t-statistic.</li>
                        <li>The density plot of the 5000 t-statistics is in blue.  This plot gives a pretty good estimate of the actual distribution of the t-statistic, for the population and sample size that you have selected.</li>
                    </ul>"))
      ) # end tabset panel
    ) # end conditonal panel
    
  ) # end main panel
  
))
