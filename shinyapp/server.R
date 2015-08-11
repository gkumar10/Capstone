library(shiny)
source('nextword.R')

shinyServer(
  
  function(input, output) {
    #define output text
    output$nextword <- renderUI({
      if (input$sentence == "") {
        HTML('Please type at least 2 words in the text box.')
      } else
        HTML(nextword(input$sentence)[1], '<br>',
             nextword(input$sentence)[2], '<br>',
             nextword(input$sentence)[3], '<br>',
             nextword(input$sentence)[4]
             )
   })
})

