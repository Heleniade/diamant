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
      selectInput(
        inputId = "CouleurDiamant",
        label = "Choisir la couleur des diamants",
        choices = c("D", "E", "F", "G", "H", "I", "J"),
        selected = "D"
      ),
      selectInput(
        inputId = "CouleurNuage",
        label = "Choisir la couleur du nuage de points",
        choices = c(
          "Bleu" = "blue", "Rouge" = "red", "Rose" = "pink", "Noir" = "black",
          "Orange" = "orange", "Vert" = "green", "Jaune" = "yellow", "Gris" = "grey"
        ),
        selected = "Bleu"
      ),
      sliderInput("Prix",
        "Prix:",
        min = 300,
        max = 19000,
        value = 326
      ),
      actionButton(
        inputId = "boutton",
        label = "Afficher les détails"
      )
    ),
    mainPanel(
      textOutput("DiamondTitle"),
      plotOutput("DiamondPlot"),
      DTOutput("DiamondTable"),
    )
  )
)



server <- function(input, output) {
  rv <- reactiveValues()
  observeEvent(c(input$Prix, input$CouleurDiamant), {
    rv$FiltreDiamant <- Diamant |>
      filter(price > input$Prix) |>
      filter(color == input$CouleurDiamant)
  })
  output$DiamondPlot <- renderPlot({
    rv$FiltreDiamant |>
      ggplot(aes(x = carat, y = price)) +
      geom_point(color = input$CouleurNuage)
  })
  output$DiamondTitle <- renderText({
    rv$FiltreDiamant |>
      nrow()

    glue("Prix : {input$Prix} & Couleur du diamant : {input$CouleurDiamant}")
  })
  output$DiamondTable <- renderDT({
    rv$FiltreDiamant
  })
  observeEvent(input$boutton, {
    showNotification(glue("Prix minimum : {input$Prix}"),
      type = "message"
    )
    showNotification(glue("Couleur du diamant : {input$CouleurDiamant}"),
      type = "message"
    )
  })
}



shinyApp(ui = ui, server = server)
