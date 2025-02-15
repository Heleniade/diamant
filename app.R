library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(glue)


Diamant <- diamonds

thematic_shiny(
  font = "auto"
)

ui <- fluidPage(
  theme= bs_theme(version = 5, bootswatch = "vapor"),
  titlePanel("Exploration des diamants"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "CouleurDiamant",
        label = "Choisir la couleur des diamants",
        choices = c("D", "E", "F", "G", "H", "I", "J"),
        selected = "D"
      ),
      radioButtons(
        inputId = "ChangerBleu",
        label = "Forcer la couleur en bleu ?",
        choices = c("Oui" = "yes", "Non" = "no"),
        selected = "no"
      ),
      sliderInput("Prix",
        "Prix maximum:",
        min = 300,
        max = 19000,
        value = 5000
      ),
      actionButton(
        inputId = "boutton",
        label = "Afficher les détails"
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
    rv$FiltreDiamant <- Diamant %>%
      filter(price < input$Prix) %>%
      filter(color == input$CouleurDiamant)
  })


  output$DiamondPlot <- renderPlot({
    couleur <- ifelse(input$ChangerBleu == "yes", "blue", "grey")
    ggplot(rv$FiltreDiamant, aes(x = carat, y = price)) +
      geom_point(color = couleur)
  })


  output$DiamondTitle <- renderText({
    n <- nrow(rv$FiltreDiamant)
    glue("Prix maximum : {input$Prix} & Couleur du diamant : {input$CouleurDiamant}")
  })

  output$DiamondTable <- renderDT({
    rv$FiltreDiamant
  })

  observeEvent(input$boutton, {
    showNotification(glue("Prix maximum : {input$Prix}"),
      type = "message"
    )
    showNotification(glue("Couleur du diamant : {input$CouleurDiamant}"),
      type = "message"
    )
  })
}

shinyApp(ui, server)
