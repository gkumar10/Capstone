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
             nextword(input$sentence)[1], '<br><br>',
             'Other choices: <br>',
             nextword(input$sentence)[2], '<br>',
             nextword(input$sentence)[3], '<br>',
             nextword(input$sentence)[4], '<br>',
             nextword(input$sentence)[5], '<br>',
             nextword(input$sentence)[6]
             )
   })
})

