library(shiny)
library(ggplot2)
source("function.R")


ui <- fluidPage(
  textInput(inputId = "num_etab", label = "Entrer le numéro d'établissement ou ALL pour avoir le population détenus agrégée", 
            placeholder = "Numéro ou ALL", value = "ALL"),
  numericInput(inputId = "mois", label = "Nombre de mois avant aujourd'hui que l'on veut afficher avant le forecast", value = 1, min = 1),
  plotOutput(outputId =  "plot"),
  #tableOutput("table")
  #textOutput("text")
)

server <- function(input, output) {
  TS <- reactive({
    penit_to_ts(input$num_etab, 2016)
  })
  X13 <- reactive({
    ts_to_X13(TS())
  })
  # data <- reactive({
  #   X13()$final$forecasts
  # })
  observe({
    print("ça marche")
  })
  output$plot <- renderPlot({
    plot(X13())
  })
 }

shinyApp(ui, server)
