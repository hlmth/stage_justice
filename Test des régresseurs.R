library(rjd3toolkit)
library(RJDemetra)
library(car)
source("function.R")


#### Test sur les régresseurs des working et trading days ####

FR <- national_calendar(list(
  special_day("NEWYEAR"),
  special_day("EASTERMONDAY"), # Lundi de Pâques
  special_day("MAYDAY"), # 1er mai
  special_day("ASCENSION"), # Jour de l'Ascension
  fixed_day(5, 8),
  special_day("WHITMONDAY"), # Lundi de Pentecôte
  fixed_day(7, 14),
  special_day("ASSUMPTION"), # Assomption
  special_day("ALLSAINTSDAY"), # Toussaint
  special_day("ARMISTICE")
))

detenus <- penit_to_2ts("France", year = 2016)[[1]]
condamnes <- penit_to_2ts("France", year = 2016)[[2]]
prevenus <- penit_to_2ts("France", year = 2016)[[3]]
MA <- penit_to_2ts("France", year = 2016, MA = TRUE)[[2]]
Autres <- penit_to_2ts("France", year = 2016, MA = TRUE)[[3]]




frequency <- frequency(detenus)
start <- start(detenus)
end <- end(detenus)
length <- (end[1] - start[1]) * 12 + end[2] - start[2] + 1

ly <- lp_variable(frequency = frequency, start = start, length = length)

reg6 <- calendar_td(FR, frequency = frequency, start = start, length = length,
                    groups = c(1, 2, 3, 4, 5, 6, 0))
reg5 <- calendar_td(FR, frequency = frequency, start = start, length = length,
                    groups = c(1, 2, 3, 4, 5, 0, 0))
reg3 <- calendar_td(FR, frequency = frequency, start = start, length = length,
                    groups = c(1, 2, 2, 2, 2, 0, 0))
reg2 <- calendar_td(FR, frequency = frequency, start = start, length = length,
                    groups = c(1, 1, 1, 1, 1, 2, 0))
reg1 <- calendar_td(FR, frequency = frequency, start = start, length = length,
                    groups = c(1, 1, 1, 1, 1, 0, 0))


regresseurs_JO <- ts(cbind(reg1, reg2, reg3, reg5, reg6),
                     start = start, frequency = frequency)
regresseurs_JO <- ts.union(regresseurs_JO,
                           ly)
colnames(regresseurs_JO) <- c("REG1_semaine",
                              sprintf("REG2_%s", c("lundi_a_vendredi", "samedi")),
                              sprintf("REG3_%s", c("lundi", "mardi_a_vendredi")),
                              sprintf("REG5_%s", c("lundi", "mardi", "mercredi", "jeudi", "vendredi")),
                              sprintf("REG6_%s", c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi")),
                              "leap_year")


# on garde le jeu reg6
wkd_1 <- regresseurs_JO[,c(grep("REG1", colnames(regresseurs_JO), value = TRUE),
                         "leap_year")]
wkd_2 <- regresseurs_JO[,c(grep("REG2", colnames(regresseurs_JO), value = TRUE),
                         "leap_year")]
wkd_3 <- regresseurs_JO[,c(grep("REG3", colnames(regresseurs_JO), value = TRUE),
                         "leap_year")]
wkd_5 <- regresseurs_JO[,c(grep("REG5", colnames(regresseurs_JO), value = TRUE),
                         "leap_year")]
wkd_6 <- regresseurs_JO[,c(grep("REG6", colnames(regresseurs_JO), value = TRUE),
                         "leap_year")]


# mais ce n'est pas obligatoire
colnames(wkd_1) <- gsub("REG1_", "", colnames(wkd_1))
colnames(wkd_2) <- gsub("REG2_", "", colnames(wkd_2))
colnames(wkd_3) <- gsub("REG3_", "", colnames(wkd_3))
colnames(wkd_5) <- gsub("REG5_", "", colnames(wkd_5))
colnames(wkd_6) <- gsub("REG6_", "", colnames(wkd_6))

myspec1 <- regarima_spec_x13(spec = "RG5c",
                             usrdef.varEnabled = TRUE,
                             usrdef.var = wkd_1,
                             usrdef.varType = "Calendar",
                             easter.enabled = FALSE)
myspec2 <- regarima_spec_x13(spec = "RG5c",
                             usrdef.varEnabled = TRUE,
                             usrdef.var = wkd_2,
                             usrdef.varType = "Calendar",
                             easter.enabled = FALSE)
myspec3 <- regarima_spec_x13(spec = "RG5c",
                             usrdef.varEnabled = TRUE,
                             usrdef.var = wkd_3,
                             usrdef.varType = "Calendar",
                             easter.enabled = FALSE)
myspec5 <- regarima_spec_x13(spec = "RG5c",
                             usrdef.varEnabled = TRUE,
                             usrdef.var = wkd_5,
                             usrdef.varType = "Calendar",
                             easter.enabled = FALSE)
myspec6 <- regarima_spec_x13(spec = "RG5c",
                             usrdef.varEnabled = TRUE,
                             usrdef.var = wkd_6,
                             usrdef.varType = "Calendar",
                             easter.enabled = FALSE)
list_spec <- list(myspec1, myspec2, myspec3, myspec5, myspec6)
myreg1 <- regarima(detenus, myspec2)
myreg2 <- regarima(condamnes, myspec1)
myreg3 <- regarima(prevenus, myspec1)
myreg4 <- regarima(MA, myspec1)
myreg5 <- regarima(Reste, myspec1)
summary(myreg1)
summary(myreg2)
summary(myreg3)
summary(myreg4)
summary(myreg5)

m <- data.frame(row.names = c("detenus", "condamnes", "prevenus", "MA", "Autres")) 
for (i in c(1:length(list_spec))){
  myreg1 <- row.names(regarima(detenus, list_spec[[i]])$regression.coefficients)
  df_det <- data.frame(type = "detenus", spec = i, myreg1) %>% 
    group_by(spec, type) %>% 
    summarize(list=list(myreg1), .groups = "keep") %>% 
    ungroup()
  myreg2 <- row.names(regarima(condamnes, list_spec[[i]])$regression.coefficients)
  df_cond <- data.frame(type = "condamnes", spec = i, myreg2) %>% 
    group_by(spec, type) %>% 
    summarize(list=list(myreg2), .groups = "keep") %>% 
    ungroup()
  myreg3 <- row.names(regarima(prevenus, list_spec[[i]])$regression.coefficients)
  df_prev <- data.frame(type = "prevenus", spec = i, myreg3) %>% 
    group_by(spec, type) %>% 
    summarize(list=list(myreg3), .groups = "keep") %>% 
    ungroup()
  myreg4 <- row.names(regarima(MA, list_spec[[i]])$regression.coefficients)
  df_MA <- data.frame(type = "MA", spec = i, myreg4) %>% 
    group_by(spec, type) %>% 
    summarize(list=list(myreg4), .groups = "keep") %>% 
    ungroup()
  myreg5 <- row.names(regarima(Autres, list_spec[[i]])$regression.coefficients)
  df_autres <- data.frame(type = "Autres", spec = i, myreg5) %>% 
    group_by(spec, type) %>% 
    summarize(list=list(myreg5), .groups = "keep") %>% 
    ungroup()
  df_ad <- rbind(df_det, df_cond, df_prev, df_MA, df_autres)
  # row.names(df_ad) <- c("detenus", "condamnes", "prevenus", "MA", "Autres")
  m <- rbind(m, df_ad)
}
#Au regard de m, la non significativité des workings days et la seule significativité des trading days pour 
#la série des condamnés (répondant à des condamnations ayant majoritairement lieu en semaine)
#et la série Autres (ne répondant à rien puisque agrégat de séries ayant surement une saisonnaité propre mais
#trop faible en stock pour être décomposée) ne permettent pas de prouver l'utilité de régresseurs spécifiques 
#pour dépendre l'impact des jours de la semaine. En effet, les autres régresseurs ne représentant pas une saisonnalité
#en lien avec la temporalité des séries, malgré leur rare significativité, ils ne sont pas à prendre en compte. 
#Mis à part le changement des jours fériés aucun régresseurs ne sera changé par rapport au modèle initiale 
#de X13. 

#### Test sur le régresseur TC ####

TC <- function(alpha, t , t_0){
  if (t<t_0){
    return(0)
  }
  else {
    return(alpha^(t-t_0))
  }
}

s <- seq(-10, 50)

l <- NULL
for (i in c(1:length(s))){
  l <- c(l,TC(0.7, s[[i]], 0))
}
plot(y = l, x = s, type = "l")
lines(y = rep(0.1, length(s)), x = s, type = "l")

list_alpha <- seq(0.7, 1, by = 0.01)

l1 <- NULL
for (i in c(1: length(list_alpha))){
  l <- NULL
  for (j in c(1:length(s))){
    l <- c(l,TC(list_alpha[[i]], s[[j]], 0))
  }
  l1 <- c(l1, which.min(abs(l - 0.1)) - 11)
  
}

m <- data.frame(alpha = list_alpha, nbr_de_période = l1 )




