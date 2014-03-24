library(shiny)

# Define UI for FindRegLine application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Can You Find the Regression Line?"),
  
  # Sidebar
  sidebarPanel(
    
    helpText("How good are you at finding where the regression line lies?",
             "Adjust the sliders below to change the y-intercept and the",
             "slope of the solid line."),
    
    uiOutput("aslider"),
    
    uiOutput("bslider"),
    
    br(),
    
    helpText("When you are satisfied with the line, press the button below."),
    
    br(),
    
    actionButton("submit","Submit New Guess"),
    
    br(),
    br(),
    
    helpText("Each press of the button counts as one turn.  Your score is the sum",
             "of the number of turns you have taken so far and a 'closeness' measurement.",
             "(The closeness measurement is always 100 at the start of the game, and can ",
             "get as low as zero if your line matches the regression line exactly.)",
              "Therefore, lower scores are better!"),
    
    br(),
    
    checkboxInput("enditall",label="I quit:  show the regression line!",
                  value=FALSE)

    
  ),
  
  
  # Here comes the main panel
  
  mainPanel(
    
    
    conditionalPanel(
      condition="!input.enditall",
      plotOutput("gamecloud")
    ),
    
    conditionalPanel(
      condition="input.enditall",
      plotOutput("finalcloud")
    ),
    
      tableOutput("score"),
    
    conditionalPanel(
      condition="input.enditall",
      tableOutput("revelation")
    )
    
    
  )
  
))
