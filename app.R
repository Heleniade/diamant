library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(glue)


Diamant <- ggplot2::diamonds

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"), 
  titlePanel("Exploration des diamants"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "CouleurDiamant",
        label = "Choisir la couleur des diamants",
        choices = unique(Diamant$color),
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
                  min = min(Diamant$price),
                  max = max(Diamant$price),
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
  rv <- reactiveValues(FiltreDiamant = Diamant)
  
  observe({
    rv$FiltreDiamant <- Diamant |>
      filter(price <= input$Prix, color == input$CouleurDiamant)
  })
  
  output$DiamondPlot <- renderPlot({
    couleur <- ifelse(input$ChangerBleu == "yes", "blue", "grey")
    ggplot(rv$FiltreDiamant, aes(x = carat, y = price)) +
      geom_point(color = couleur)
  })
  
  output$DiamondTitle <- renderText({
    glue("Prix maximum : {input$Prix} & Couleur du diamant : {input$CouleurDiamant}")
  })
  
  output$DiamondTable <- renderDT({
    datatable(rv$FiltreDiamant, options = list(pageLength = 10))
  })
  
  observeEvent(input$boutton, {
    showNotification(glue("Prix maximum : {input$Prix}"), type = "message")
    showNotification(glue("Couleur du diamant : {input$CouleurDiamant}"), type = "message")
  })
}

shinyApp(ui, server)
