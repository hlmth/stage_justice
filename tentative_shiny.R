library(shiny)
library(ggplot2)
source("function.R")


ui <- fluidPage(
  textInput(inputId = "num_etab", label = "Entrer le numéro d'établissement ou ALL pour avoir le population détenus agrégée", 
            placeholder = "Numéro ou ALL", value = "ALL"),
  numericInput(inputId = "mois", label = "Nombre de mois avant aujourd'hui que l'on veut afficher avant le forecast", value = 1, min = 1),
  plotOutput(outputId =  "plot"),
  #tableOutput("table")
)

server <- function(input, output) {
  output$plot <- renderPlot({
    df <- str_to_df(input$num_etab,
                    input$mois)
    plot(df$fcst)
  })
 }

shinyApp(ui, server)
