library(haven)
library(dplyr)
library(utils)
library(forecast)
library(urca)
library(readxl)
library(tidyverse)
library(ggplot2) #pour le graphe
library(knitr)
library(RJDemetra)

mens_aggreg <- read_sas("~/work/mens_agreg.sas7bdat")

MA <- mens_aggreg %>% filter(quartier_etab == "MA/QMA") %>% group_by(dt_mois) %>%  summarise(detenus_MA = sum(detenus))

A <- mens_aggreg %>% group_by(dt_mois) %>%  summarise(detenus_MA = sum(detenus))
ALL <- penit_to_ts("ALL")

tsma <- ts(MA$detenus_MA, start = c(2004, 3), frequency = 12)
plot(tsma)
class(ALL)
x13_outl <- x13_spec(spec = c("RSA5c"),
                     usrdef.outliersEnabled = TRUE,
                     usrdef.outliersType = rep("TC", 14),
                     usrdef.outliersDate = as.character(MA$dt_mois[194:207]),
                     transform.function = "Auto")

x13_model <- x13(ALL, x13_outl) # X-13ARIMA method
ts_model <- tramoseats(tsma) # TRAMO-SEATS method

# Basic plot with the original series, the trend and the SA series
plot(x13_model, type_chart = "sa-trend")

# S-I ratio
plot(x13_model$decomposition)
layout(matrix(1:6, 3, 2));plot(x13_model$regarima, ask = FALSE);layout(matrix(1))
dev.off()
summary(x13_model$regarima)
x13_model$regarima$forecast



