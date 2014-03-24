library(shiny)

# Define UI for SlowGoodness application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Chi-Square Goodness-of-Fit Resampling"),
  
  # Sidebar
  sidebarPanel(
    textInput("nulls","Enter Null Probabilities (separated by commas)",
              "0.17,0.17,0.17,0.17,0.17,0.17"),
    
    helpText("Enter the probabilities as decimal numbers.",
              "If they do not sum to 1, then the",
             "application will re-scale them for you."),
    br(),

    textInput("obs","Enter Observed Counts (separated by commas)",
              "8,18,11,7,9,7"),
    br(),
    
    textInput("names","Enter Level Names (separated by commas)",
              "One,Two,Three,Four,Five,Six"),
    br(),
    helpText("Before you begin resampling, make sure that the",
             "number of Null Probabilities that you entered matches",
             "the number of observed counts!"),
    actionButton("resample","Resample Now"),
    actionButton("reset","Start Over")
    ),

  
  # Here comes the main panel
  
  mainPanel(
    
    
    conditionalPanel(
      condition="output.resampcount == 0",
      plotOutput("bargraphinit"),
      h4(textOutput("remark0")),
      tableOutput("obstable")
      ),
    
    conditionalPanel(
      condition="output.resampcount == 1",
      plotOutput("bargraph1"),
      h4(textOutput("remark1"))),
    
    conditionalPanel(
      condition="output.resampcount > 1",
    tabsetPanel(selected="Observed Results",
      tabPanel("Observed Results",
                 plotOutput("bargraph"),
               h4(textOutput("remark2")),
               tableOutput("resampstats")),
      tabPanel("Density Plot (Resamples)",
               plotOutput("constructden"),
               tableOutput("tableden"),
               tableOutput("summaryden")),
      id="MyPanel"
    )
    )
    
    
  )
  
))
