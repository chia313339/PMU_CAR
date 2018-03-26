library(shiny)
shinyUI(fluidPage(
  titlePanel("大家好"),
  sidebarLayout(
    sidebarPanel(
      # radioButtons("radio", label = "Choices", choices = list("Choice 1" = 1, "Choice 2" = 2)),
      # sliderInput("slider1", label = "Slider", min = 0, max = 100, value = 50),
      # # Copy the line below to make a text input box
      textInput("text", label = h3("Text input"), value = "Enter text..."),
      numericInput("reports", label = h3("Reports input"), value = 0),
      numericInput("age", label = h3("Age input"), value = 20),
      numericInput("income", label = h3("Income input"), value = 2.2),
      radioButtons("owner", label = h3("Owner"),
                   choices = list("yes" = 'yes', "no" = 'no'), 
                   selected = 'no'),
      numericInput("months", label = h3("Months input"), value = 12)
 
    ),
    
    mainPanel(
      verbatimTextOutput('value')
      )
    
  )
))