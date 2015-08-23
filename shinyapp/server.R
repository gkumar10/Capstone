library(shiny)
source('nextword.R')

shinyServer(
  
  function(input, output) {
    #define output text
    output$nextword <- renderUI({
      if (input$sentence == "") {
        HTML('Please type at least 1 word in the text box.')
      } else {
          predictedwords <- nextword(input$sentence)
          HTML('<b>Best choice of next word:</b> <br>',
             ifelse(is.na(predictedwords[1]), "No prediction of next word. Please try a different phrase.", predictedwords[1]), '<br><br>',
             ifelse(is.na(predictedwords[2]) | !input$otherwords, "", '<b>Other word choices:</b> <br>'),
             ifelse(is.na(predictedwords[2]) | !input$otherwords, "", predictedwords[2]), '<br>',
             ifelse(is.na(predictedwords[3]) | !input$otherwords, "", predictedwords[3]), '<br>',
             ifelse(is.na(predictedwords[4]) | !input$otherwords, "", predictedwords[4]), '<br>',
             ifelse(is.na(predictedwords[5]) | !input$otherwords, "", predictedwords[5]), '<br>',
             ifelse(is.na(predictedwords[6]) | !input$otherwords, "", predictedwords[6]) 
        )
      
      }
   })
})

