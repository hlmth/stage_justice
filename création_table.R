#Création des tables
library(openxlsx)


mens_aggreg <- read_sas("~/work/mens_agreg.sas7bdat")

df <- mens_aggreg %>% 
  filter(year(dt_mois) >= 2016) %>% 
  group_by(dt_mois) %>% 
  summarise(detenus = sum(detenus), condamnes = sum(condamnes_detenus, condamnes_prevenus), prevenus = sum(prevenus))
View(df)
#df <- head(df, nrow(df) - 1) #pour enlever un mois (utile pour faire l'intérprétation de l'écart)
write.xlsx(df, "TS_Condamnés_Prévenus.xlsx")

