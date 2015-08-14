library(shiny)
source('nextword.R')

shinyServer(
  
  function(input, output) {
    #define output text
    output$nextword <- renderUI({
      if (input$sentence == "") {
        HTML('Please type at least 1 word in the text box.')
      } else
        HTML('Best choice of next word: <br>',
             ifelse(is.na(nextword(input$sentence)[1]), "No prediction of next word. Please try a different phrase.", nextword(input$sentence)[1]), '<br><br>',
             ifelse(is.na(nextword(input$sentence)[2]), "", 'Other choices: <br>'),
             ifelse(is.na(nextword(input$sentence)[2]), "", nextword(input$sentence)[2]), '<br>',
             ifelse(is.na(nextword(input$sentence)[3]), "", nextword(input$sentence)[3]), '<br>',
             ifelse(is.na(nextword(input$sentence)[4]), "", nextword(input$sentence)[4]), '<br>',
             ifelse(is.na(nextword(input$sentence)[5]), "", nextword(input$sentence)[5]), '<br>',
             ifelse(is.na(nextword(input$sentence)[6]), "", nextword(input$sentence)[6])
             )
   })
})

