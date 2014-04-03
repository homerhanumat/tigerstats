library(shiny)




# Define UI for testing application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Confidence Intervals for a Population Mean"),
  
  # Sidebar
  sidebarPanel(
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
             "within the confidence interval?   Enter a number between 50 and 99 for",
             "percent-confidence level."),
    
    numericInput(inputId="confLevel","Confidence Level",value=95,min=50,max=99,step=1),
    br(),
    
    helpText("You can get just one sample, with a histogram of the sample and a picture",
             "of the confidence interval. or you can get 5000 samples, with a summary",
             "of how many times the interval contained the population mean."),
    
    radioButtons(inputId="actionType","One at a Time or 5000 at Once?",
                 list("Just One Sample, Please"="one",
                      "Give me 5000 samples!"="fiveThousand")),
    br(),
    
    helpText("When you are ready to go, push this button:"),
    actionButton("go","Take the Sample(s)")
  ),
  
  
  # Here comes the main panel
  
  mainPanel(
    
    conditionalPanel(
      condition="output.go == 0",
      plotOutput("initialGraph")
    ),
    
    conditionalPanel(
      condition="(output.go > 0) && (output.actionType == 'one')",
      plotOutput("graphSample"),
      HTML("<p> </p>"),
      HTML("<ul>
                <li>The population density curve is in red.</li>
                <li>The vertical line marks the population mean.</li>
                <li>The histogram of the sample is in light blue.</li>
                <li>The sample mean is the big blue dot.</li>
                <li>The confidence interval is in green.</li>
          </ul>")
    ),
    
    conditionalPanel(
      condition="(output.go > 0) && (output.actionType == 'fiveThousand')",
      plotOutput("initialGraph2"),
      tableOutput("summary"),
      dataTableOutput("intervalFrame")
    )
    
  )
  
))
