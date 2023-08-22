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

tabl_reg <- data.frame(row.names = c("detenus", "condamnes", "prevenus", "MA", "Autres")) 
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
  tabl_reg <- rbind(tabl_reg, df_ad)
}
#Au regard de tabl_reg, la non significativité des workings days et la seule significativité des trading days pour 
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
temps
s <- seq(-10, 100) #ensemble des périodes temporelles

list_alpha <- seq(0.7, 1, by = 0.01) #on créé la liste des alphas pour lesquels on va chercher le nbr de période
l1 <- NULL ; l2 <- NULL ; l4 <- NULL
for (i in c(1: length(list_alpha))){
  l <- NULL
  for (j in c(1:length(s))){ # on crée la liste des valeurs de TC pour t appartenant à s et pour un certain alpha appartentnat à list_alpha
    l <- c(l,TC(list_alpha[[i]], s[[j]], 0))
  }
  l1 <- c(l1, which.min(abs(l - 0.3)) - 11) #on regarde en combien de période la fonction TC << 1 (on le traduit par TC < 0.3)
  var1 <- NULL
  for (j in c(1:length(temps))){
    var1 <- c(var1,TC(list_alpha[[i]], temps[[j]], 52))
  }
  var1 <- ts(var1, start = start(detenus), frequency = 12)
  l4 <- c(l4, var1[[53]])
  X13_spec_TC1 <- x13_spec(spec = "RSA5c",
                           usrdef.varEnabled = TRUE,
                           usrdef.var = var1,
                           usrdef.varType = "Irregular",
                           transform.function = "Auto",
                           easter.enabled = FALSE,
                           outlier.enabled = FALSE )
  X13_TC1 <- x13(detenus, X13_spec_TC1)
  l2 <- c(l2, round(as.numeric(X13_TC1$regarima$loglik[5])))
}

tabl_prd <- data.frame(alpha = list_alpha, nbr_de_période = l1 , aicc = l2)

#D'après la table période, on remarque que l'alpha le plus pértinent pour réprésenter l'éffet d'un temporary
#change sur 13 périodes est alpha = 0.91, on trouve que pour l'alpha usuel de 0.7 représente un effet
#de temporary change significatif sur 7 période. On peut donc remplacer les outliers que l'on a implémenter pour
#l'instant par un seul outliers TC en période 194 ou 52 (2020-04-01) ou un outliers A0 en période 194 ou 52 et un TC avec
#un alpha = 0.9 en 195 ou 53 (2020-05-01), le plus bas du pic atteint pendant la période COVID. 

detenus <- penit_to_2ts("France", year = 2016)[[1]]
condamnes <- penit_to_2ts("France", year = 2016)[[2]]
prevenus <- penit_to_2ts("France", year = 2016)[[3]]
MA <- penit_to_2ts("France", year = 2016, MA = TRUE)[[2]]
Autres <- penit_to_2ts("France", year = 2016, MA = TRUE)[[3]]

list_TS <- list(detenus, condamnes, prevenus, MA, Autres)

date_outl <- ts_to_outl(detenus) #On créé la spécification utilisée jusqu'ici
X13_spec_ini <- x13_spec(spec = c("RSA5c"),
                         usrdef.outliersEnabled = TRUE,
                         usrdef.outliersType = c(rep("AO", 2),rep("TC", 12)),
                         usrdef.outliersDate = date_outl,
                         transform.function = "Auto",
                         easter.enabled = FALSE)

temps <- seq(1, length(detenus)) #ensemble des périodes temporelles

var1 <- NULL
for (i in c(1:length(temps))){
  var1 <- c(var1,TC(0.91, temps[[i]], 52))
}
var1 <- ts(var1, start = start(detenus), frequency = 12) #On créé le régresseur TC 0.91 avec la valeur du alpha déterminée précédement

var2 <- NULL
for (i in c(1:length(temps))){
  var2 <- c(var2,TC(0.9, temps[[i]], 53))
}
var2 <- ts(var2, start = start(detenus), frequency = 12) #On créé le régresseur TC avec la valeur du alpha déterminée précédement
                                                         #qui sera complété avec un outliers AO

X13_spec_TC <- x13_spec(spec = "RSA5c",
                         usrdef.varEnabled = TRUE,
                         usrdef.var = var1,
                         usrdef.varType = "Irregular",
                         transform.function = "Auto",
                         easter.enabled = FALSE,
                         outlier.enabled = FALSE )

X13_spec_TCAO <- x13_spec(spec = "RSA5c",
                          tradingdays.option = "None",
                           usrdef.outliersEnabled = TRUE,
                           usrdef.outliersType = "AO",
                           usrdef.outliersDate = "2020-04-01",
                           usrdef.varEnabled = TRUE,
                           usrdef.var = var2,
                           usrdef.varType = "Irregular",
                           transform.function = "Auto",
                           easter.enabled = FALSE,
                           outlier.enabled = FALSE)
f <- function(t, t_0,t_1){  #Au vu des courbes des séries temporelles étudiées, on essaye un régresseur linéaire
  if (t<t_0 |t>t_1 ){
    return(0)
  }
  else {
    return(abs(t - t_1)/(t_1-t_0))
  }
}
var3 <- NULL
for (i in c(1:length(temps))){
  var3 <- c(var3,f(temps[[i]], 52, 73))
}
var3 <- ts(var3, start = start(detenus), frequency = 12) 
plot(var3)
X13_spec_lin <- x13_spec(spec = "RSA5c",
                         usrdef.varEnabled = TRUE,
                         usrdef.var = var3,
                         usrdef.varType = "Irregular",
                         transform.function = "Auto",
                         easter.enabled = FALSE,
                         outlier.enabled = FALSE )

tabl_aicc <- data.frame(row.names = c("detenus", "condamnes", "prevenus", "MA", "Autres"))
tabl_aicc[["aicc sans TC"]] <- rep(NA, 5)
tabl_aicc[["aicc avec TC O.91"]] <- rep(NA, 5)
tabl_aicc[["aicc avec TC 0.9 et AO"]] <- rep(NA, 5)
tabl_aicc[["aicc avec reg linéaire"]] <- rep(NA, 5)
tabl_aicc[["aicc auto"]] <- rep(NA, 5)
for (i in seq(1:length(list_TS))){
  X13_ini <- x13(list_TS[[i]], X13_spec_ini)
  X13_TC <- x13(list_TS[[i]], X13_spec_TC)
  X13_TCAO <- x13(list_TS[[i]], X13_spec_TCAO)
  X13_lin <- x13(list_TS[[i]], X13_spec_lin)
  X13_Auto <- x13(list_TS[[i]], spec = "RSA5c")
  a <- round(as.numeric(X13_ini$regarima$loglik[5]))
  b <- round(as.numeric(X13_TC$regarima$loglik[5]))
  c <- round(as.numeric(X13_TCAO$regarima$loglik[5]))
  d <- round(as.numeric(X13_lin$regarima$loglik[5]))
  e <- round(as.numeric(X13_Auto$regarima$loglik[5]))
  tabl_aicc[i,] <- c(a, b, c, d, e)}

plot(detenus)
lines((var3)*-13000 +71000)

#Tous les types de régresseurs testés ici sont significatif sur l'ensemble des séries au moins 
#au seuil de 1%. 
#Au vue de la tabl_aicc le modèle avec régresseur linéaire n'a jamais l'aicc le plus faible des 3
#modèles testés ici. Par ailleurs, aucune des deux représentations du covid ne semble être meilleure 
#que l'autre le minimum entre l'un et l'autre dépendant de la série. 
