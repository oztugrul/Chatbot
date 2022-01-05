library(shiny)

shinyUI(fluidPage(
  titlePanel("Yardım Menüsü"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("test_review_data","Lütfen Sorunuzu Yazınız.",value = " ",width="250px",rows=10),      
      submitButton("Submit")

    ),
    mainPanel(
      h3("Cevap:"),
      textOutput("pred1")
    )
  )
))
