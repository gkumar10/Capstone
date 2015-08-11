library(shiny)

#shiny app for next word prediction
#coursera data science capstone project

text1 <- "Type a phrase with at least 1 word"
text2 <- "Click 'Predict Next Word' button."

shinyUI(
  pageWithSidebar(
    
    #set header text
    headerPanel("Next Word Prediction"),
  
    #fill side panel with help text (instructions), 2 list boxes and 1 submit button
    sidebarPanel(
      h4("This Shiny app will predict next word in your phrase."),
      helpText(strong("Instructions:")),
      helpText(strong(tags$ol(tags$li(text1), tags$li(text2)))),
      h2("Input:"),
      textInput("sentence", label="Phrase", value=""),
      submitButton('Predict Next Word')
      ),
    
    mainPanel(
      
      #show output
      h2("Choices for next word:"),
      uiOutput("nextword")
      
    )
  )
)