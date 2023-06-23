library(haven)
library(dplyr)

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
        filter(mens_aggreg, cd_etablissement = str) %>%
        group_by(dt_mois) %>%
        summarise(detenus = sum(detenus))
      return(ts(TS$detenus, start = c(year(min(TS$dt_mois)), month(min(TS$dt_mois))), frequency = 12))
    } else {
      
      quartiers <- unique(filter(mens_aggreg, cd_etablissement == str)$quartier_etab)
      for (qua in quartiers) {
        
        
        
      } 
    }
  }
  else {
    return("L'établissement n'est plus ouvert ou le format de saisie n'est pas bon.")
  }
  
}
class(year(min(TS$dt_mois)))
TS <- mens_aggreg %>%
  filter(cd_etablissement == numT) %>%
  group_by(dt_mois) %>%
  summarise(detenus = sum(detenus))
View(TS)
plot(ts(TS$detenus, start = c(2004, 3), frequency = 12))
filter(d, cd_etablissement == numT5)$nb_quartier == 5
numF <- "00919923"
numT <- "00908908"
numT5 <- "00912148"
numF %in% etab_ouvert
numT %in% etab_ouvert
c(numF, numT) %in% etab_ouvert
d <- mens_aggreg %>%
  filter(cd_etablissement %in% etab_ouvert) %>%
  distinct(cd_etablissement, lc_etab, type_etab, quartier_etab) %>%
  group_by(cd_etablissement) %>%
  mutate(nb_quartier = n_distinct(quartier_etab)) 
d[d$cd_etablissemnt == numT]
d <- unique(d[-4][-3])
length(unique(d$cd_etablissement))
last_month <- max(mens_aggreg$dt_mois)
etab_ouvert <- unique(filter(mens_aggreg, dt_mois == last_month)$cd_etablissement)
mens_aggreg <-rev(mens_aggreg)

