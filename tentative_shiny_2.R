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
        tabPanel("Forecast",
                 h2("Prévisions du nombre de détenus décomposés en condamnés et prévenus"),
                 tableOutput("tabledet")),
        tabPanel("Model",
                 h2("Modèle X13_Arima pour la série temporelle des détenus"),
                 verbatimTextOutput("summary_det"),
                 h2("Modèle X13_Arima pour la série temporelle des condamnés"),
                 verbatimTextOutput("summary_cond"),
                 h2("Modèle X13_Arima pour la série temporelle des prévenus"),
                 verbatimTextOutput("summary_prev"))
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
             usrdef.outliersType = c(rep("AO", 2),rep("TC", 12)),
             usrdef.outliersDate = date_outl(),
             transform.function = "Auto")
  })
  list_x13_modele <- reactive({
    list(x13(list_TS()[[1]], x13_outl()), #detenus
         x13(list_TS()[[2]], x13_outl()), 
         x13(list_TS()[[3]], x13_outl()))
  })
  observe({
    print(list_x13_modele()[[1]]$regarima$forecast[,2])
  })
  output$plot_detenus <- renderPlot({
    sum_mod_plt(list(list_x13_modele()[[2]],
                     list_x13_modele()[[3]]),
      list_x13_modele()[[1]], input$mois)
  })
  output$plot_condamnes <- renderPlot({
    mod_to_plt(list_x13_modele()[[2]], input$mois)
  })
  output$plot_prevenus <- renderPlot({
    mod_to_plt(list_x13_modele()[[3]], input$mois)
  })
  s <- reactive({
    as.Date(str_to_time(paste(start(list_x13_modele()[[1]]$regarima$forecast)[1],
          start(list_x13_modele()[[1]]$regarima$forecast)[2], "01", sep = "-")))
  })
  ld <- reactive({
    s() %m+% months(23)
  })
  output$tabledet <- renderTable({
    ts_fcst1 <- list_x13_modele()[[1]]$regarima$forecast
    ts_fcst2 <- list_x13_modele()[[2]]$regarima$forecast
    ts_fcst3 <- list_x13_modele()[[3]]$regarima$forecast
    data.frame(date = format(seq(s(),ld(), by = 'month'), "%Y-%m-%d"),
               forecast_detenus = sprintf("%.2f ± %.2f",
                                  as.numeric(ts_fcst1[,1]),
                                  as.numeric(ts_fcst1[,2])),
               forecast_condamnes = sprintf("%.2f ± %.2f",
                                  as.numeric(ts_fcst2[,1]),
                                  as.numeric(ts_fcst2[,2])),
               forecast_prevenus = sprintf("%.2f ± %.2f",
                                  as.numeric(ts_fcst3[,1]),
                                  as.numeric(ts_fcst3[,2])))
  })
  # output$tablecond <- renderTable({
  #   ts_fcst <- list_x13_modele()[[2]]$regarima$forecast
  #   data.frame(date = format(seq(s(),ld(), by = 'month'), "%Y-%m-%d"),
  #              forecast = sprintf("%.2f ± %.2f",
  #                                 as.numeric(ts_fcst[,1]),
  #                                 as.numeric(ts_fcst[,2])))
  # })
  # output$tableprev <- renderTable({
  #   data.frame(date = format(seq(s(),ld(), by = 'month'), "%Y-%m-%d"),
  #              forecast = sprintf("%.2f ± %.2f",
  #                                 as.numeric(ts_fcst[,1]),
  #                                 as.numeric(ts_fcst[,2])))
  # })
  list_summary_mod <- reactive({
    list(summary(list_x13_modele()[[1]]$regarima),
         summary(list_x13_modele()[[2]]$regarima),
         summary(list_x13_modele()[[3]]$regarima))
  })
  # output$table1 <- renderTable({
  #   data.frame(table_mod()$results_spec)
  # })
  # output$table2 <- renderTable({
  #   t(data.frame(table_mod()$arma_orders))
  # })
  # output$table3 <- renderTable({
  #   data.frame(table_mod()$coefficients$arima)
  # })
  # output$table4 <- renderTable({
  #   data.frame(table_mod()$coefficients$regression)
  # })
  # output$table5 <- renderTable({
  #   t(data.frame(table_mod()$loglik))
  # })
  output$summary_det <- renderPrint({
    print(list_summary_mod()[[1]])
  })
  output$summary_cond <- renderPrint({
    print(list_summary_mod()[[2]])
  })
  output$summary_prev <- renderPrint({
    print(list_summary_mod()[[3]])
  })
}

shinyApp(ui, server)
