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
  list_outl <- reactive({
    ts_to_X13(TS())
  })
  x13_outl <- reactive({
    x13_spec(spec = c("RSA5c"),
             usrdef.outliersEnabled = TRUE,
             usrdef.outliersType = rep("TC", length(list_outl())),
             usrdef.outliersDate = date[list_outl()],
             transform.function = "Auto")
  })
  x13_modele <- reactive({
    x13(TS(), x13_outl())
  })
  observe({
    print(x13_modele()$regarima$forecast[,2])
  })
  output$plot <- renderPlot({
    str_to_df(x13_modele(), input$mois)
  })
}

shinyApp(ui, server)
