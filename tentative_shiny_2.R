library(shiny)
library(ggplot2)
library(knitr)
source("function2.R")


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "num_etab", label = "Entrer le numéro d'établissement ou ALL pour avoir le population détenus agrégée", 
                placeholder = "Numéro ou ALL", value = "ALL"),
      numericInput(inputId = "mois", label = "Nombre de mois avant aujourd'hui que l'on veut afficher avant le forecast", value = 1, min = 1)
    ),
    mainPanel(
      plotOutput(outputId = "plot_detenus"),
      plotOutput(outputId = "plot_condamnes"),
      plotOutput(outputId = "plot_prevenus"),
      tabsetPanel(
        tabPanel("a", tableOutput("table1"), tableOutput("table5")),
        tabPanel("b", tableOutput("table2")),
        tabPanel("c", uiOutput("table3")),
        tabPanel("d", tableOutput("table4")),
      )
    )
  )
)

server <- function(input, output) {
  list_TS <- reactive({
    penit_to_2ts(input$num_etab, 2016)
  })
  date_outl <- reactive({
    ts_to_outl(list_TS()[[1]])
  })
  x13_outl <- reactive({
    x13_spec(spec = c("RSA5c"),
             usrdef.outliersEnabled = TRUE,
             usrdef.outliersType = rep("TC", length(date_outl())),
             usrdef.outliersDate = date_outl(),
             transform.function = "Auto")
  })
  list_x13_modele <- reactive({
    list(x13(list_TS()[[1]], x13_outl()),
         x13(list_TS()[[2]], x13_outl()),
         x13(list_TS()[[3]], x13_outl()))
  })
  observe({
    print(list_x13_modele()[[1]]$regarima$forecast[,2])
  })
  output$plot_detenus <- renderPlot({
    list_mod_to_plt(list(
      
      list_x13_modele()[[1]]), input$mois)
  })
  output$plot_condamnes <- renderPlot({
    mod_to_plt(list_x13_modele()[[2]], input$mois)
  })
  output$plot_prevenus <- renderPlot({
    mod_to_plt(list_x13_modele()[[3]], input$mois)
  })
  # table <- reactive({
  #   summary(x13_modele()$regarima)
  # })
  # output$table1 <- renderTable({
  #   data.frame(table()$results_spec)
  # })
  # output$table2 <- renderTable({
  #   t(data.frame(table()$arma_orders))
  # })
  # output$table3 <- renderUI({
  #   kables(table()$coefficients)
  # })
  # output$table4 <- renderTable({
  #   t(data.frame(table()$loglik))
  # })
  # output$table5 <- renderTable({
  #   t(data.frame(table()$residuals_st_err))
  # })
}

shinyApp(ui, server)
