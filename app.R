library(shiny)
library(ggplot2)
library(dplyr)
library(glue)
library(DT)


Diamant <- diamonds

ui <- fluidPage(
  titlePanel("Exploration des diamants"),
  selectInput(
    inputId = "couleur",
    label = "Choisir la couleur",
    choices = c(
      "Bleu" = "blue","Rouge" = "red","Rose" = "pink","Noir" = "black",
      "Orange" = "orange","Vert" = "green","Jaune" = "yellow","Gris"="grey"
    ),
    selected = "Bleu"
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput("prix",
        "Prix:",
        min = min(Diamant$price),
        max = max(Diamant$price),
        value = min(Diamant$price)
      )
    ),
    mainPanel(
      plotOutput("DiamondPlot")
    )
  )
)


server <- function(input, output) {
  output$DiamondPlot <- renderPlot({
    Diamant |>
      filter(price > input$prix) |>
      ggplot(aes(x = carat, y = price)) +
      geom_point(color = input$couleur) # Directly use input$couleur
  })
}



shinyApp(ui = ui, server = server)
