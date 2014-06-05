library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Predictiveness Curve Illustration"),
  
  sidebarPanel(
    sliderInput("vl", "Low risk threshold:", 
                value = 0, min = 0, max = 1, ticks = F), 
    sliderInput("vh", "High risk threshold:", 
                value = 1, min = 0, max = 1, ticks = F),
    
    verbatimTextOutput("summary")
        
    ),
  
  mainPanel(
    plotOutput("predcurvePlot", height = "600px", width = "auto",
               clickId = "onClick", hoverId = "onHover", 
               hoverDelayType = "debounce")
    
    )
  
  
  ))