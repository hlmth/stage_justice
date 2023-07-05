library(shiny)
source("function.R")


ui <- fluidPage(
  textInput(inputId = "num_étab", label = "Entrer le numéro d'établissement ou ALL pour avoir le population détenus agrégée", 
            placeholder = "Numéro ou ALL", value = "ALL"),
  numericInput(inputId = "mois", label = "Nombre de mois avant aujourd'hui que l'on veut afficher avant le forecast", value = 0),
  plotOutput(outputId =  "plot_série")
)

server <- function(input, output) {
 
  output$plot_série <- renderText(input$mois)
  t <- observe({input$mois}) #nombre de mois que l'on veut afficher avant le forecast


  TS <- penit_to_ts(observe({input$num_étab}), 2016)
  x13_model <- ts_to_X13(TS)
  ts_fcst <- x13_model$regarima$forecast
  ts_s<- tail(x13_model$final$series[,1], t)

  s <- start(ts_s)
  s <- paste(s[1],s[2], "01", sep = "-")
  sd <- as.Date(str_to_time(s)) #date de début
  ld <- sd %m+% months(t + 23)
  df <- data.frame(date = seq(1, t), fcst = as.numeric(ts_s), stderr_fcst = NA)
  df2 <- data.frame(date = seq(t + 1, t + 24), fcst = as.numeric(ts_fcst[,1]), stderr_fcst = as.numeric(ts_fcst[,2]))
  df <- df %>%
    rbind(df2) %>%
    mutate(date = seq(sd, ld, by = "month"))

  ouput$plot_série <- renderPlot({ggplot(data = df, aes(x = date, y = fcst)) +
    geom_line() +
    labs(x = "Evolution mensuelle", y = "Nombre de détenus") +
    scale_x_continuous(breaks = seq(sd, ld, by = "3 month"), labels = seq(sd, ld, by = "3 month")) +
    theme(axis.text.x = element_text(angle = 305, vjust = 0.5)) +
    geom_ribbon( aes (ymin = fcst - stderr_fcst, ymax = fcst + stderr_fcst), alpha = 0.2)
  })
 }

shinyApp(ui, server)
