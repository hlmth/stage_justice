library(haven)
library(dplyr)
library(utils)
library(tidyverse)
library(RJDemetra)



penit_to_2ts <- function(str, year = 0, MA = FALSE){
  # mens_aggreg <- read_sas("~/work/mens_agreg.sas7bdat")
  DISP <- c("DISP BORDEAUX", "DISP DIJON", "DISP LILLE", "DISP LYON", "DISP MARSEILLE", "DISP PARIS", "DISP RENNES", "DISP STRASBOURG", "DISP TOULOUSE", "MOM", "DSPOM")
  if (str == "France" & MA == FALSE) {
    ALL <- mens_aggreg %>%
      filter(year(dt_mois) >= year) %>%
      group_by(dt_mois) %>%
      summarise(detenus = sum(detenus), condamnes = sum(condamnes_detenus, condamnes_prevenus), prevenus = sum(prevenus))
    return(list(ts(ALL$detenus, start = c(year(min(ALL$dt_mois)), month(min(ALL$dt_mois))), frequency = 12),
                ts(ALL$condamnes, start = c(year(min(ALL$dt_mois)), month(min(ALL$dt_mois))), frequency = 12),
                ts(ALL$prevenus, start = c(year(min(ALL$dt_mois)), month(min(ALL$dt_mois))), frequency = 12)))
  }
  if (str == 'France' & MA == TRUE){
    MA <- mens_aggreg %>% 
      mutate(pivot = ifelse(quartier_etab == "MA/QMA", "detenus_MA", "detenus_reste")) %>% 
      select(dt_mois, detenus, pivot) %>% 
      group_by(dt_mois, pivot) %>%
      mutate(detenus = sum(detenus)) %>%
      ungroup() %>%
      distinct() %>%
      pivot_wider(names_from = pivot, values_from = detenus) %>%
      mutate(detenus = detenus_MA + detenus_reste) %>% 
      filter(year(dt_mois) >= year)
    return(list(ts(MA$detenus, start = c(year(min(MA$dt_mois)), month(min(MA$dt_mois))), frequency = 12),
                ts(MA$detenus_MA, start = c(year(min(MA$dt_mois)), month(min(MA$dt_mois))), frequency = 12),
                ts(MA$detenus_reste, start = c(year(min(MA$dt_mois)), month(min(MA$dt_mois))), frequency = 12)))
  }
  if (str %in% DISP & MA == FALSE){
    DISP <- mens_aggreg %>%
      filter(year(dt_mois) >= year & lc_disp == str) %>%
      group_by(dt_mois) %>%
      summarise(detenus = sum(detenus), condamnes = sum(condamnes_detenus, condamnes_prevenus), prevenus = sum(prevenus))
    return(list(ts(DISP$detenus, start = c(year(min(DISP$dt_mois)), month(min(DISP$dt_mois))), frequency = 12),
                ts(DISP$condamnes, start = c(year(min(DISP$dt_mois)), month(min(DISP$dt_mois))), frequency = 12),
                ts(DISP$prevenus, start = c(year(min(DISP$dt_mois)), month(min(DISP$dt_mois))), frequency = 12)))
  }
  if (str %in% DISP & MA == TRUE){
    DISP <- mens_aggreg %>% 
      mutate(pivot = ifelse(quartier_etab == "MA/QMA", "detenus_MA", "detenus_reste")) %>% 
      select(dt_mois, detenus, pivot, lc_disp) %>% 
      filter(year(dt_mois) >= year & lc_disp == str) %>% 
      group_by(dt_mois, pivot) %>%
      mutate(detenus = sum(detenus)) %>%
      ungroup() %>%
      distinct() %>%
      pivot_wider(names_from = pivot, values_from = detenus) %>%
      mutate(detenus = detenus_MA + detenus_reste) 

    return(list(ts(DISP$detenus, start = c(year(min(DISP$dt_mois)), month(min(DISP$dt_mois))), frequency = 12),
                ts(DISP$detenus_MA, start = c(year(min(DISP$dt_mois)), month(min(DISP$dt_mois))), frequency = 12),
                ts(DISP$detenus_reste, start = c(year(min(DISP$dt_mois)), month(min(DISP$dt_mois))), frequency = 12)))
  }
  last_month <- max(mens_aggreg$dt_mois) #dernier mois apparaissant dans mens_aggreg
  etab_ouvert <- filter(mens_aggreg, dt_mois == last_month)$lc_etab #liste des établissements ouvert le mois dernier
  d <- mens_aggreg %>%
    filter(lc_etab %in% etab_ouvert)%>%
    distinct(cd_etablissement, lc_etab, type_etab, quartier_etab) %>%
    group_by(lc_etab) %>%
    mutate(nb_quartier = n_distinct(quartier_etab)) 
  d <- unique(d[-4][-3])
  if (str %in% etab_ouvert){
    if (filter(d, lc_etab == str)$nb_quartier == 1){
      TS <- mens_aggreg %>%
        filter(lc_etab == str & year(dt_mois)>= year) %>%
        group_by(dt_mois) %>%
        summarise(detenus = sum(detenus), condamnes = sum(condamnes_detenus, condamnes_prevenus), prevenus = sum(prevenus))
      return(list(ts(TS$detenus, start = c(year(min(TS$dt_mois)), month(min(TS$dt_mois))), frequency = 12),
                  ts(TS$condamnes, start = c(year(min(TS$dt_mois)), month(min(TS$dt_mois))), frequency = 12),
                  ts(TS$prevenus, start = c(year(min(TS$dt_mois)), month(min(TS$dt_mois))), frequency = 12)))
    } else {
      l_ts <- list()
      quartiers <- unique(filter(mens_aggreg, lc_etab == str)$quartier_etab)
      for (qua in quartiers) {
        TS <- mens_aggreg %>%
          filter(lc_etab == str & quartier_etab == qua & year(dt_mois) >= year) %>%
          group_by(dt_mois) %>%
          summarise(detenus = sum(detenus), condamnes = sum(condamnes_detenus, condamnes_prevenus), prevenus = sum(prevenus))
        l_ts[[qua]] <- list(ts(TS$detenus, start = c(year(min(TS$dt_mois)), month(min(TS$dt_mois))), frequency = 12),
                            ts(TS$condamnes, start = c(year(min(TS$dt_mois)), month(min(TS$dt_mois))), frequency = 12),
                            ts(TS$prevenus, start = c(year(min(TS$dt_mois)), month(min(TS$dt_mois))), frequency = 12))
      }
      return(l_ts)
    }
  }
  else {
    return("L'établissement n'est plus ouvert ou le format de saisie n'est pas bon.")
  }
}

str_to_time <- function(str){
  date_str <- ifelse(str_length(str)==9 ,str_replace(str,"-","-0"),str)
  return(date_str)
}

ts_to_outl <- function(ts){
  date <- time(ts); year <- floor(date); month <- round((date - floor(date)) * 12)+1
  date <- paste(year, month, "01", sep = "-") 
  df <- data.frame(date) %>% 
    mutate(date=str_to_time(date))
  date <- df$date 
  COVID_seq <- seq(as.Date("2020-04-01"), as.Date("2021-05-01"), by = "month") %>% as.character()
  list_outl <- which(date %in% COVID_seq)
  return(date[list_outl])
}


# list_mod_to_plt <- function(list_model, t){
#   l <- list()
#   for (i in seq(1, length(list_model))){
#     x13_model <- list_model[[i]]
#     ts_fcst <- x13_model$regarima$forecast
#     ts_s<- tail(x13_model$final$series[,1], t)
#     
#     
#     s <- start(ts_s)
#     s <- paste(s[1],s[2], "01", sep = "-")
#     sd <- as.Date(str_to_time(s)) #date de début
#     ld <- sd %m+% months(t + 23)
#     df <- data.frame(date = seq(1, t), fcst = as.numeric(ts_s), stderr_fcst = NA)
#     df2 <- data.frame(date = seq(t + 1, t + 24), fcst = as.numeric(ts_fcst[,1]), stderr_fcst = as.numeric(ts_fcst[,2]))
#     df <- df %>%
#       rbind(df2) %>%
#       mutate(date = seq(sd, ld, by = "month"))
#     l[[i]] <- df
#   }
#   df <- rbind(l[[1]], l[[2]], l[[3]])
#   df$group <- rep(c("df1", "df2", "df3"), each = nrow(l[[1]]) )
#   return(ggplot(data = df, aes(x = date, y = fcst, color = group)) +
#            geom_line() +
#            geom_line() +
#            geom_line() +
#            labs(x = "Evolution mensuelle", y = "Nombre de détenus") +
#            scale_x_continuous(breaks = seq(sd, ld, by = "3 month"), labels = seq(sd, ld, by = "3 month")) +
#            theme(axis.text.x = element_text(angle = 305, vjust = 0.5)) +
#            geom_ribbon( aes (ymin = fcst - stderr_fcst, ymax = fcst + stderr_fcst), alpha = 0.2))
# }

mod_to_df <- function(model, t){
  x13_model <- model
  ts_fcst <- x13_model$regarima$forecast
  ts_s<- tail(x13_model$final$series[,1], t)
  
  
  s <- start(ts_fcst)
  s <- paste(s[1],s[2], "01", sep = "-")
  s <- as.Date(str_to_time(s))
  s <- s %m-% months(t)
  ld <- s %m+% months(t + 23)
  df <- data.frame(date = seq(1, t), fcst = as.numeric(ts_s), stderr_fcst = NA)
  df2 <- data.frame(date = seq(t + 1, t + 24), fcst = as.numeric(ts_fcst[,1]), stderr_fcst = as.numeric(ts_fcst[,2]))
  df <- df %>%
    rbind(df2) %>%
    mutate(date = seq(s, ld, by = "month"))
  return(df)
}

sum_mod_plt <- function(list_mod, model, t, nom_1, nom_2){
  x13_model <- model
  ts_fcst <- x13_model$regarima$forecast
  ts_s<- tail(x13_model$final$series[,1], t)
  
  s <- start(ts_fcst)
  s <- paste(s[1],s[2], "01", sep = "-")
  s <- as.Date(str_to_time(s))
  s <- s %m-% months(t)
  ld <- s %m+% months(t + 23)
  df1 <- data.frame(date = seq(1, t), fcst = as.numeric(ts_s), stderr_fcst = NA)
  df2 <- data.frame(date = seq(t + 1, t + 24), fcst = as.numeric(ts_fcst[,1]), stderr_fcst = NA)
  df_mod <- df1 %>%
    rbind(df2) %>%
    mutate(date = seq(s, ld, by = "month"))
  df <- data.frame(matrix(0, nrow = 24 + t, ncol = 3))
  for (i in seq(1, length(list_mod))){
    x13_model <- list_mod[[i]]
    ts_fcst <- x13_model$regarima$forecast
    ts_s<- tail(x13_model$final$series[,1], t)
    
    s <- start(ts_fcst)
    s <- paste(s[1],s[2], "01", sep = "-")
    s <- as.Date(str_to_time(s))
    s <- s %m-% months(t)
    ld <- s %m+% months(t + 23)
    df1 <- data.frame(date = seq(1, t), fcst = as.numeric(ts_s), stderr_fcst = NA)
    df2 <- data.frame(date = seq(t + 1, t + 24), fcst = as.numeric(ts_fcst[,1]), stderr_fcst = as.numeric(ts_fcst[,2]))
    df1 <- df1 %>%
      rbind(df2) 
    df <- df1 + df
  }
  df <- df %>%
    mutate(date = seq(s, ld, by = "month")) %>%
    rbind(df_mod)
  df$group <- rep(c(nom_1, nom_2), each = nrow(df_mod) )
  return(df)
}

