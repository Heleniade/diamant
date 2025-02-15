library(shiny)
library(ggplot2)
library(dplyr)
library(glue)
library(DT)


Diamant <- diamonds

ui <- fluidPage(
  titlePanel("Exploration des diamants"),
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

  sidebarLayout(
    sidebarPanel(
      sliderInput("Prix",
        "Prix:",
        min = min(Diamant$price), 
        max = max(Diamant$price),
        value = min(Diamant$price)
      )
    ),
    mainPanel(
      textOutput("DiamondTitle"),
      plotOutput("DiamondPlot"),
      DTOutput("DiamondTable")
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
}



shinyApp(ui = ui, server = server)
