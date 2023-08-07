library(shiny)
library(ggplot2)
library(knitr)
library(kableExtra) 
source("function.R")

choix <- c("Condamnés/Prévenus", "MA/Autres")
mens_aggreg <- read_sas("~/work/mens_agreg.sas7bdat")
last_month <- max(mens_aggreg$dt_mois) #dernier mois apparaissant dans mens_aggreg
etab_ouvert <- filter(mens_aggreg, dt_mois == last_month)$lc_etab %>% #liste des établissements ouvert le mois dernier
  as.data.frame() 
DISP <- c("France", "DISP BORDEAUX", "DISP DIJON", "DISP LILLE", "DISP LYON", "DISP MARSEILLE", "DISP PARIS", "DISP RENNES", "DISP STRASBOURG", "DISP TOULOUSE", "MOM", "DSPOM") %>%
  as.data.frame()
etab_ouvert <- rbind(DISP, etab_ouvert)

ui <- fluidPage(
  sidebarLayout(
    ################################### Sidebar #####################################
    sidebarPanel(
      selectInput("type", "Type de décomposition du nombre de détenus", choix),
      conditionalPanel("input.type == 'Condamnés/Prévenus'",
                       selectInput(inputId = "num_etab", label = "Choisir l'établissement ou France pour avoir le population détenus agrégée", etab_ouvert)),
      conditionalPanel("input.type == 'MA/Autres'",
                       selectInput(inputId = "num_etab_MA", label = "Choisir l'établissement ou France pour avoir le population détenus agrégée", DISP)),
      numericInput(inputId = "mois", label = "Nombre de mois avant aujourd'hui que l'on veut afficher avant le forecast", value = 24, min = 1)
      
    ),
    ################################### Main Panel #####################################
    
    mainPanel(
      tabsetPanel(
        tabPanel("Graphiques",
                 conditionalPanel("input.type == 'Condamnés/Prévenus'",
                                  plotOutput(outputId = "plot_detenus"),
                                  plotOutput(outputId = "plot_condamnes"),
                                  plotOutput(outputId = "plot_prevenus")),
                 conditionalPanel("input.type == 'MA/Autres'",
                                  plotOutput(outputId = "plot_detenus_MA"),
                                  plotOutput(outputId = "plot_MA"),
                                  plotOutput(outputId = "plot_Autres"))),
        tabPanel("Forecast",
                 conditionalPanel("input.type == 'Condamnés/Prévenus'",
                                  h2("Prévisions du nombre de détenus décomposés en condamnés et prévenus")),
                 conditionalPanel("input.type == 'MA/Autres'",
                                  h2("Prévisions du nombre de détenus décomposés en détenus en maison d'arrêt 
                                     et en détenus dans un autre type de quartier pénitentiaire")),
                 tableOutput("tabledet")),
        tabPanel("Model",
                 h2("Modèle X13_Arima pour la série temporelle des détenus"),
                 verbatimTextOutput("summary_det"),
                 conditionalPanel("input.type == 'Condamnés/Prévenus'",
                                  h2("Modèle X13_Arima pour la série temporelle des condamnés")),
                 conditionalPanel("input.type == 'MA/Autres'",
                                  h2("Modèle X13_Arima pour la série temporelle des détenus en maison d'arrêt")),
                 verbatimTextOutput("summary_cond"),
                 conditionalPanel("input.type == 'Condamnés/Prévenus'",
                                  h2("Modèle X13_Arima pour la série temporelle des prévenus")),
                 conditionalPanel("input.type == 'MA/Autres'",
                                  h2("Modèle X13_Arima pour la série temporelle des détenus dans un autre type de quartier pénitentiaire")),
                 verbatimTextOutput("summary_prev"))
      )
    )
    #####
  )
)




server <- function(input, output){
  ################################### Obtention TS et modèles #####################################
  observe({print("ok")})
  list_TS_CP <- reactive({if (input$type == 'Condamnés/Prévenus')
    penit_to_2ts(input$num_etab, 2016, MA = FALSE) 
  })
  list_TS <- reactive({if (input$type == 'MA/Autres')
    penit_to_2ts(input$num_etab_MA, 2016, MA = TRUE) else list_TS_CP()
  })
  date_outl <- reactive({
    ts_to_outl(list_TS()[[1]])
  })
  x13_outl <- reactive({
    x13_spec(spec = c("RSA5c"),
             usrdef.outliersEnabled = TRUE,
             usrdef.outliersType = c(rep("AO", 2),rep("TC", 12)),
             usrdef.outliersDate = date_outl(),
             transform.function = "Auto",
             easter.enabled = FALSE)
  })
  list_x13_modele <- reactive({
    list(x13(list_TS()[[1]], x13_outl()), #detenus
         x13(list_TS()[[2]], x13_outl()), 
         x13(list_TS()[[3]], x13_outl()))
  })
  observe({
    print(list_x13_modele()[[2]]$regarima$forecast[,1])
  })
  ################################### Graphiques #####################################
  ### Détenus/Condamnés ###
  output$plot_detenus <- renderPlot({
    df <- sum_mod_plt(list(list_x13_modele()[[2]],
                           list_x13_modele()[[3]]),
                      list_x13_modele()[[1]], input$mois,
                      "Moyenne des Forecasts condamnés plus prévenus",
                      "Détenus")
    ggplot(data = df, aes(x = date, y = fcst, color = group)) +
      geom_line() +
      geom_line() +
      labs(x = "Evolution mensuelle", y = "Nombre de détenus") +
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "4 month") +      
      theme(axis.text.x = element_text(angle = 305, vjust = 0.5)) +
      geom_ribbon( aes (ymin = fcst - stderr_fcst, ymax = fcst + stderr_fcst), alpha = 0.2)
  })
  output$plot_condamnes <- renderPlot({
    df <- mod_to_df(list_x13_modele()[[2]], input$mois)
    ggplot(data = df, aes(x = date, y = fcst)) +
      geom_line() +
      labs(x = "Evolution mensuelle", y = "Nombre de condamnés") +
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "4 month") +      
      theme(axis.text.x = element_text(angle = 305, vjust = 0.5)) +
      geom_ribbon( aes (ymin = fcst - stderr_fcst, ymax = fcst + stderr_fcst), alpha = 0.2)
  })
  output$plot_prevenus <- renderPlot({
    df <- mod_to_df(list_x13_modele()[[3]], input$mois)
    ggplot(data = df, aes(x = date, y = fcst)) + 
      geom_line() +
      labs(x = "Evolution mensuelle", y = "Nombre de prévenus") +
      geom_ribbon( aes (ymin = fcst - stderr_fcst, ymax = fcst + stderr_fcst), alpha = 0.2) +
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "4 month") +
      theme(axis.text.x = element_text(angle = 305, vjust = 0.5))
  })
  ### MA/Autres ###
  output$plot_detenus_MA <- renderPlot({
    df <- sum_mod_plt(list(list_x13_modele()[[2]],
                           list_x13_modele()[[3]]),
                      list_x13_modele()[[1]], input$mois,
                      "Moyenne des Forecasts détenus en maison d'arrêt plus
                      détenus dans un autre type de quartier pénitentiaire",
                      "Détenus")
    ggplot(data = df, aes(x = date, y = fcst, color = group)) +
      geom_line() +
      geom_line() +
      labs(x = "Evolution mensuelle", y = "Nombre de détenus") +
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "4 month") +      
      theme(axis.text.x = element_text(angle = 305, vjust = 0.5)) +
      geom_ribbon( aes (ymin = fcst - stderr_fcst, ymax = fcst + stderr_fcst), alpha = 0.2)
  })
  
  output$plot_MA <- renderPlot({
    df <- mod_to_df(list_x13_modele()[[2]], input$mois)
    ggplot(data = df, aes(x = date, y = fcst)) +
      geom_line() +
      labs(x = "Evolution mensuelle", y = "Nombre de détenus en maison d'arrêt") +
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "4 month") +      
      theme(axis.text.x = element_text(angle = 305, vjust = 0.5)) +
      geom_ribbon( aes (ymin = fcst - stderr_fcst, ymax = fcst + stderr_fcst), alpha = 0.2)
  })
  output$plot_Autres <- renderPlot({
    df <- mod_to_df(list_x13_modele()[[3]], input$mois)
    ggplot(data = df, aes(x = date, y = fcst)) + 
      geom_line() +
      labs(x = "Evolution mensuelle", y = "Nombre de détenus dans un autre type de quartier pénitentiaire") +
      geom_ribbon( aes (ymin = fcst - stderr_fcst, ymax = fcst + stderr_fcst), alpha = 0.2) +
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "4 month") +
      theme(axis.text.x = element_text(angle = 305, vjust = 0.5))
  })
  ################################### Forecast#####################################
  
  s <- reactive({
    as.Date(str_to_time(paste(start(list_x13_modele()[[1]]$regarima$forecast)[1],
                              start(list_x13_modele()[[1]]$regarima$forecast)[2], "01", sep = "-")))
  })
  ld <- reactive({
    s() %m+% months(23)
  })
  # output$tabledet <- renderTable({
  #   ts_fcst1 <- list_x13_modele()[[1]]$regarima$forecast
  #   ts_fcst2 <- list_x13_modele()[[2]]$regarima$forecast
  #   ts_fcst3 <- list_x13_modele()[[3]]$regarima$forecast
  #   data.frame(date = format(seq(s(),ld(), by = 'month'), "%Y-%m-%d"),
  #              forecast_detenus = sprintf("%.2f ± %.2f",
  #                                         as.numeric(ts_fcst1[,1]),
  #                                         as.numeric(ts_fcst1[,2])),
  #              forecast_condamnes = sprintf("%.2f ± %.2f",
  #                                           as.numeric(ts_fcst2[,1]),
  #                                           as.numeric(ts_fcst2[,2])),
  #              forecast_prevenus = sprintf("%.2f ± %.2f",
  #                                          as.numeric(ts_fcst3[,1]),
  #                                          as.numeric(ts_fcst3[,2])))
  # })
  tablecol <- reactive({
    if (input$type == 'Condamnés/Prévenus') c(" ", 
                                              "Forecast détenus",
                                              "Forecast condamnés",
                                              "Forecast prévenus",
                                              "Somme des forecasts de la décomposition") 
    else c(" ", 
           "Forecast détenus",
           "Forecast des détenus en maison d'arrêt",
           "Forecast des détenus dans les autres types de quartier de détention",
           "Somme des forecasts de la décomposition") 
  })
  output$tabledet <- function(){
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
                                           as.numeric(ts_fcst3[,2])),
               somme = sprintf("%.2f ± %.2f",
                               as.numeric(ts_fcst2[,1]) + as.numeric(ts_fcst3[,1]),
                               as.numeric(ts_fcst2[,2]) + as.numeric(ts_fcst3[,2]))) %>% 
      knitr::kable(col.names = tablecol()) %>% 
      kable_styling("striped", full_width = F)
  }
  ################################### Summary des modèles #####################################
  
  list_summary_mod <- reactive({
    list(summary(list_x13_modele()[[1]]$regarima),
         summary(list_x13_modele()[[2]]$regarima),
         summary(list_x13_modele()[[3]]$regarima))
  })
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