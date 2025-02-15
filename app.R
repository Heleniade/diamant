library(shiny)
library(ggplot2)
library(dplyr)
library(glue)
library(DT)


Diamant <- diamonds

ui <- fluidPage(
  titlePanel("Exploration des diamants"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("prix",
        "Prix:",
        min = min(Diamant$price),
        max = max(Diamant$price),
        value = 1
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
      geom_point()
  })
}


shinyApp(ui = ui, server = server)
