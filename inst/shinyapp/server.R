library(shiny)
library(predcurver)


shinyServer(function(input, output){
  
  risk.est <- runif(200)
  
  vlout <- reactive({
        if(input$vl %in% c(0,1)) return(.5) else return(input$vl)
      })
  vhout <- reactive({
        if(input$vh %in% c(0,1)) return(.5) else return(input$vh)
      })
    
  output$predcurvePlot <- renderPlot({
    predc <- predcurve(risk.est)
    plot(predc, vl = vlout(), vh = vhout(), lwd = 2)
    lines(predc, lwd = 2, type = 's')
    
    if(!is.null(input$onHover) && 
         input$onHover$y > evalcdf(predc, input$onHover$x)){
    toy <- input$onHover$y  #evalcdf(predc, input$onHover$x)
    tox <- evalcdf(predc[,2:1], input$onHover$y)
    segments(c(0, tox), c(input$onHover$y, 0), 
             c(tox, tox), c(input$onHover$y, toy), 
             lwd = 1, col = "lightblue")
    }
    if(!is.null(input$onHover) && 
         input$onHover$y <= evalcdf(predc, input$onHover$x)){
    toy <- evalcdf(predc, input$onHover$x)
    tox <- input$onHover$x
    segments(c(0, tox), c(toy, 0), 
             c(input$onHover$x, input$onHover$x), c(toy, toy), 
             lwd = 1, col = "lightblue")
          
    }
    
  })
  
  output$summary <- renderPrint({
    
    quiet <- summary(predcurve(risk.est), vl = vlout(), vh = vhout())
        
  })
  
  
  
})