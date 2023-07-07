library(haven)
library(dplyr)
library(utils)
library(tidyverse)
penit_to_ts <- function(str, year = 0){
  mens_aggreg <- read_sas("~/work/mens_agreg.sas7bdat")
  if (str == "ALL") {
    ALL <- mens_aggreg %>%
      filter(year(dt_mois) >= year) %>%
      group_by(dt_mois) %>%
      summarise(detenus = sum(detenus))
    return(ts(ALL$detenus, start = c(year(min(ALL$dt_mois)), month(min(ALL$dt_mois))), frequency = 12))
  }
  last_month <- max(mens_aggreg$dt_mois) #dernier mois apparaissant dans mens_aggreg
  etab_ouvert <- filter(mens_aggreg, dt_mois == last_month)$cd_etablissement #liste des établissements ouvert le mois dernier
  d <- mens_aggreg %>%
    filter(cd_etablissement %in% etab_ouvert)%>%
    distinct(cd_etablissement, lc_etab, type_etab, quartier_etab) %>%
    group_by(cd_etablissement) %>%
    mutate(nb_quartier = n_distinct(quartier_etab)) 
  d <- unique(d[-4][-3])
  if (str %in% etab_ouvert){
    if (filter(d, cd_etablissement == str)$nb_quartier == 1){
      TS <- mens_aggreg %>%
        filter(cd_etablissement == str & year(dt_mois)>= year) %>%
        group_by(dt_mois) %>%
        summarise(detenus = sum(detenus))
      return(ts(TS$detenus, start = c(year(min(TS$dt_mois)), month(min(TS$dt_mois))), frequency = 12))
    } else {
      l_ts <- list()
      quartiers <- unique(filter(mens_aggreg, cd_etablissement == str)$quartier_etab)
      for (qua in quartiers) {
        TS <- mens_aggreg %>%
          filter(cd_etablissement == str & quartier_etab == qua & year(dt_mois) >= year) %>%
          group_by(dt_mois) %>%
          summarise(detenus = sum(detenus))
        l_ts[[qua]] <- ts(TS$detenus, start = c(year(min(TS$dt_mois)), month(min(TS$dt_mois))), frequency = 12)
      }
      return(l_ts)
    }
  }
  else {
    return("L'établissement n'est plus ouvert ou le format de saisie n'est pas bon.")
  }
}

numF <- "00919923"
numT <- "00908908"
numT5 <- "00912148"
tsT <- penit_to_ts(numT)

str_to_time <- function(str){
  date_str <- ifelse(str_length(str)==9 ,str_replace(str,"-","-0"),str)
  return(date_str)
}

ts_to_X13 <- function(ts){
  date <- time(ts); year <- floor(date); month <- round((date - floor(date)) * 12)+1
  date <- paste(year, month, "01", sep = "-") 
  df <- data.frame(date) %>% 
    mutate(date=str_to_time(date))
  date <- df$date 
  class(date[1])
  COVID_seq <- seq(as.Date("2020-04-01"), as.Date("2021-05-01"), by = "month") %>% as.character()
  list_outl <- which(date %in% COVID_seq)
  return(list_outl)
  # x13_outl <- x13_spec(spec = c("RSA5c"),
  #                      usrdef.outliersEnabled = TRUE,
  #                      usrdef.outliersType = rep("TC", length(list_outl)),
  #                      usrdef.outliersDate = date[list_outl],
  #                      transform.function = "Auto")
  # return(x13_outl$regarima$regression$userdef$outliers$Final$date)
  # x13_model <- x13(ts, x13_outl)
  # return(x13_model$final$forecasts[,2]) # X-13ARIMA method
}

str_to_df <- function(model, t){
  # TS <- penit_to_ts(str, 2016)
  # x13_model <- ts_to_X13(TS)
  x13_model <- model
  # return(x13_model$regarima$forecast)
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
  # return(df)
  return(ggplot(data = df, aes(x = date, y = fcst)) +
    geom_line() +
    labs(x = "Evolution mensuelle", y = "Nombre de détenus") +
    scale_x_continuous(breaks = seq(sd, ld, by = "3 month"), labels = seq(sd, ld, by = "3 month")) +
    theme(axis.text.x = element_text(angle = 305, vjust = 0.5)) +
    geom_ribbon( aes (ymin = fcst - stderr_fcst, ymax = fcst + stderr_fcst), alpha = 0.2))
}


# str_to_df("ALL", 1)
