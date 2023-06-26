library(haven)
library(dplyr)
library(utils)
library(tidyverse)
penit_to_ts <- function(str){
  mens_aggreg <- read_sas("~/work/mens_agreg.sas7bdat")
  if (str == "ALL") {
    ALL <- mens_aggreg %>%
      group_by(dt_mois) %>%
      summarise(detenus = sum(detenus))
    return(ts(ALL$detenus, start = c(2004, 3), frequency = 12))
  }
  last_month <- max(mens_aggreg$dt_mois) #dernier mois apparaissant dans mens_aggreg
  etab_ouvert <- filter(mens_aggreg, dt_mois == last_month)$cd_etablissement #liste des établissements ouvert le mois dernier
  d <- mens_aggreg %>%
    filter(cd_etablissement %in% etab_ouvert) %>%
    distinct(cd_etablissement, lc_etab, type_etab, quartier_etab) %>%
    group_by(cd_etablissement) %>%
    mutate(nb_quartier = n_distinct(quartier_etab)) 
  d <- unique(d[-4][-3])
  if (str %in% etab_ouvert){
    if (filter(d, cd_etablissement == str)$nb_quartier == 1){
      TS <- mens_aggreg %>%
        filter(cd_etablissement == str) %>%
        group_by(dt_mois) %>%
        summarise(detenus = sum(detenus))
      return(ts(TS$detenus, start = c(year(min(TS$dt_mois)), month(min(TS$dt_mois))), frequency = 12))
    } else {
      l_ts <- list()
      quartiers <- unique(filter(mens_aggreg, cd_etablissement == str)$quartier_etab)
      for (qua in quartiers) {
        TS <- mens_aggreg %>%
          filter(cd_etablissement == str & quartier_etab == qua) %>%
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

