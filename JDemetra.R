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




tsma <- ts(MA$detenus_MA, start = c(2004, 3), frequency = 12)


x13_outl <- x13_spec(spec = c("RSA5c"),
                     usrdef.outliersEnabled = TRUE,
                     usrdef.outliersType = rep("TC", 21),
                     usrdef.outliersDate = as.character(MA$dt_mois[191:211]),
                     transform.function = "Auto")
x13_mod <- x13(tsma)
x13_model <- x13(tsma, x13_outl, userdefined = "diagnostics.ic-ratio") # X-13ARIMA method
ts_model <- tramoseats(tsma) # TRAMO-SEATS method

# Basic plot with the original series, the trend and the SA series
plot(x13_model, type_chart = "sa-trend")

# S-I ratio
plot(x13_model$decomposition)
layout(matrix(1:6, 3, 2));plot(x13_model$regarima, ask = FALSE);layout(matrix(1))

summary(x13_model$regarima)
x13_model$regarima$forecast

print("ok")
print("okok")
nrow(unique(mens_aggreg[c("cd_etablissement", "lc_etab", "dt_mois")][mens_aggreg$dt_mois == "2023-05-01",]))
dt <- mens_aggreg[c("cd_etablissement", "lc_etab", "type_etab", "quartier_etab")] %>%
  unique() %>% group_by(cd_etablissement)
dt <- dt[order(dt$cd_etablissement),]
dt_test <- dt[78:85,]
dt$nb_quartier <- 0
#dt_test$quartiers <- NA
i <- 1
s <- 1
#l <- NULL
while (i<353) {
  #l <- c(l,dt_test$quartier_etab[i])
  
  if (dt$cd_etablissement[i]==dt$cd_etablissement[i+1]){
    s <- s + 1
    #l <- c(l, dt_test$quartier_etab[i+1])
  } else {
    dt$nb_quartier[i] <- s
    #dt_test$quartiers[i] <- l
    s <- 1
    #l <- NULL
  }
  i <- i + 1
}
dt$nb_quartier[i] <- s
#dt_test$quartiers[i] <- l


dt <- dt %>% filter(nb_quartier != 0)





dt <- mens_aggreg %>%
  
  distinct(cd_etablissement, lc_etab, type_etab, quartier_etab) %>%
  group_by(cd_etablissement) %>%
  mutate(nb_quartier = n_distinct(quartier_etab)) 
dt <- unique(dt[-4])
length(unique(dt$cd_etablissement))

dt2 <- aggregate(quartier_etab ~ cd_etablissement, data = mens_aggreg, FUN = function(x) length(unique(x)))
names(dt2) <- c("cd_etablissement", "nb_quartier")
dt2 <- dt2[dt2$nb_quartier != 0, ]
dt2 <- dt2[order(dt2$cd_etablissement), ]
