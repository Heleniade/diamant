library(shiny)
library(ggplot2)
library(dplyr)
library(glue)
library(DT)

Diamant <- diamonds
min(Diamant$price)
ui <- fluidPage(
  titlePanel("Prix des diamants"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("prix",
        "Prix:",
        min = min(Diamant$price),
        max = max(Diamant$price),
        value = 30
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
      ggplot(aes(x = price)) +
      geom_histogram(
        binwidth = 10,
        fill = "darkgray",
        color = "white"
      )
  })
}


shinyApp(ui = ui, server = server)
