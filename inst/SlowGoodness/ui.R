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
    helpText("One simulation means the machine will produce one table of",
             "counts, using the Null probabilities.  How many simulations do",
             "you want the machine to perform at once?  (Limit is 10000.)"),
    numericInput("sims","Number of Simulations at Once",1,min=0,step=1),
    br(),
    actionButton("resample","Simulate Now"),
    actionButton("reset","Start Over")
    ),

  
  # Here comes the main panel
  
  # Here comes the main panel
  
  mainPanel(
    
    conditionalPanel(
      condition="input.resample == 0 || output.totalPrev == output.total",
      plotOutput("barGraphInitial"),
      #      p(textOutput("remarksInitial")) weird bug here I think
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
  
))