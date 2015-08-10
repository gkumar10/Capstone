library(shiny)

#The eurodist dataset give the road distances (in km) between 21 cities in Europe. The data are taken from a table in The Cambridge Encyclopaedia.

#convert eurodist to a matrix object. 
eurodist <- as.matrix(eurodist)

shinyServer(
  
  function(input, output) {
    
    #define output text
    output$distancetext <- renderUI({
             input$sentence
   })
    })