library(haven)
library(dplyr)
library(utils)
library(tseries)
library(forecast)
library(fUnitRoots)
library(kableExtra)
library(xtable)
library(urca)
library(tswge)
library(readxl)
library(tidyverse)
library(ggplot2) #pour le graphe
library(astsa) # pour voir acf et pacf
library(TSA) # pour l'eacf
library(knitr)
library(uroot) #pour le test HEGY
source("function.R")

mens_aggreg <- read_sas("~/work/mens_agreg.sas7bdat")


View(mens_aggreg)


mens_aggreg_ <- mens_aggreg %>% 
  group_by(dt_mois, quartier_etab) %>% summarise(detenus = sum(detenus), .groups = "keep")

View(mens_aggreg_)


ALL <- mens_aggreg %>%
  group_by(dt_mois) %>%
  summarise(detenus = sum(detenus))

MA <- mens_aggreg %>% filter(quartier_etab == "MA/QMA") %>% group_by(dt_mois) %>% summarise(detenus_MA = sum(detenus) )
                          
ALL_2013_2019 <- ALL[107:190,]
tsALL <- ts(ALL$detenus, start = 2004, frequency = 12)
tsALL_2013_2019 <- ts(ALL_2013_2019$detenus, start = 2013, frequency = 12)
plot(tsALL_2013_2019)

tsALL <- penit_to_2ts("France", year = 2007)[[1]]
plot(tsALL,
     xlab = "Année")
acf(tsALL)
pacf(tsALL)
monthplot(tsALL)
lag.plot (tsALL , lags =12 , layout =c(3 ,4) ,do.lines = FALSE )


plot(decompose(tsALL))


length(tsALL)
summary(lm(tsALL ~ seq(1, 197))) #on teste la pr?sence d'une tendance

#m?me chose avec un test de Dickey Fuller
test.df.trend <- ur.df(log(tsALL),
                       type = "trend", lags = 0)

res.df <- data.frame(as.vector(test.df.trend@teststat),
                     test.df.trend@cval)
names(res.df) <- c("Stat","CV 1pct", "CV 5pct", "CV 10pct")
xtable(res.df) %>%
  kable(digits=2) %>%
  kable_styling()
#On v?rifie que la tendance est bien d?terministe
test.df.trend <- ur.df(diff(log(tsma)),
                       type = "drift", lags = 1)


#Fonction utile pour plus tard
modesspectral <- function(spec)
{# L'argument spec est le r?sultat de la fonction parzen.wge
x <- spec[[2]]
n <- length(x)
aux1 <- x[2:(n-1)] - x[1:(n-2)]
aux2 <- x[2:(n-1)] - x[3:n]
aux <- (aux1>0)*(aux2>0)
specmodes <- which(aux >0)
freq <- spec[[1]][specmodes]
res <- rbind(freq,spec[[2]][specmodes],1/freq)
rownames(res) <- c("Fr?quences","Densit?","P?riodes")
res
}


diffp <- diff(log(tsALL), differences = 1)
plot(diffp)

diffs <- diff(log(tsALL), differencies = 1, lag =12)
plot(diffs)

diffsetp <- diff(diffp, differences = 1, lag = 12)
plot(diffsetp)


spectral <- parzen.wge(diffp)
modesspectral(spectral) #on a de la p?riodicit? principalement tous les ans et tous les semestres 

spectral <- parzen.wge(diffs)
modesspectral(spectral) #plus de périodicité

spectral <- parzen.wge(diffsetp)
modesspectral(spectral) #non plus

summary(lm(diffp ~ seq(1, 230))) #on a bien plus de tendance ni de constante
summary(lm(diffs ~ seq(1, 219))) #l? non plus
summary(lm(diffsetp ~ seq(1, 218))) #l? non plus

plot(decompose(diffp))
plot(decompose(diffs))
plot(decompose(diffsetp))
#on observe si la s?rie diff?renc?e pourrait sembler stationnaire
#on voit bien que la s?rie diffs ne parrait pas du tout stationnaire, 
#la saisonnalit? ne permettant pas d'expliquer toute la s?rie

adf.test(diffp)
kpss.test(diffp)
pp.test(diffp)
#
adf.test(diffs)
kpss.test(diffs)
pp.test(diffs) #on rejette l'hypoth?se nulle ? 5% contre 1% dans les autres cas
#
adf.test(diffsetp)
kpss.test(diffsetp)
pp.test(diffsetp)


# 
# adfTest_valid <- function(series,kmax,type){ #test adf jusqu'? des r?sidus non auto-corr?l?s 
# k <- 0
# noautocorr <- 0
# while (noautocorr==0){
#   cat(paste0("ADF with ",k, " lags: residuals OK? "))
#   adf <- adfTest(series,lags=k,type=type)
#   pvals <- Qtests(adf@test$lm$residuals,24,fitdf=length(adf@test$lm$coefficients))[,2]
#   if (sum(pvals<0.05,na.rm=T) == 0) {
#     noautocorr <- 1; cat("OK \n")}
#   else cat("nope \n")
#   k <- k + 1
# }
# return(adf)}
# 
# adf <- adfTest_valid(tsma,50, "ct")
# adfdf <- adfTest_valid(diffp, 24, "ct" )
# adfdfs <- adfTest_valid(diffsetp, 24, "ct" )



# acf(as.numeric(diffp), main = 'ACF de la s?rie diff?renci?e p', lag.max = 50)
# pacf(as.numeric(diffp), main= 'PACF de la s?rie diff?renci?e', lag.max = 50, show)
acf2(diffp) #c'est saisonni? 

# acf(as.numeric(diffs), main = 'ACF de la s?rie diff?renci?e s', lag.max = 50)
# pacf(as.numeric(diffs), main= 'PACF de la s?rie diff?renci?e s', lag.max = 50)
acf2(diffs) #c'est non saisonnié mais non stationnaire

# acf(as.numeric(diffsetp), main = 'ACF de la s?rie diff?renci?e s et p', lag.max = 50)
# pacf(as.numeric(diffsetp), main= 'PACF de la s?rie diff?renci?e s et p', lag.max = 50)
acf2(diffsetp) #c'est non saisonni?


tables <- data.frame(model = character(),AIC=  numeric(),BIC = numeric())
for (i in c(0:2)){
  for (j in c(0:2)){
    model <- arima(diffsetp, order  = c(i,0,j))
    aic <- round(AIC(model),3)
    bic <- round(BIC(model),3)
    tables[nrow(tables) +1, ] <- c(paste0('arima(',
                                          as.character(i),',0,',as.character(j),')'), AIC = aic, BIC = bic)
  }
}
tablep <- data.frame(model = character(),AIC=  numeric(),BIC = numeric())
for (i in c(0:2)){
  for (j in c(0:5)){
    model <- arima(diffp, order  = c(i,0,j))
    aic <- round(AIC(model),3)
    bic <- round(BIC(model),3)
    tablep[nrow(tablep) +1, ] <- c(paste0('arima(',
                                          as.character(i),',0,',as.character(j),')'), AIC = aic, BIC = bic)
  }
}



print( tables[which.min(tables$AIC),]) 
print( tables[which.min(tables$BIC),])


print( tablep[which.min(tablep$AIC),]) 
print( tablep[which.min(tablep$BIC),]) 

arima215 <- Arima(tsALL, order = c(2,1,5), include.mean = F)
sarima215 <- Arima(tsALL, order = c(2,1,5), seasonal = c(2,1,0), include.mean = F)

Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l,
                                           type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}


Qtests(sarima215$residuals, 24, fitdf = 5) 
#Rien ne rejette l'abscence d'autocorrélation des résidus, le modèle est valide.

# 
adj_r2 <- function(model){
  ss_res <- sum(model$residuals^2) #somme des résidus au carré
  p <- model$arma[1] 
  
  q <- model$arma[2]
  ss_tot <- sum(diffp[-c(1:max(p,q))]^2)
  n <- model$nobs-max(p,q) 
  adj_r2 <- 1-(ss_res/(n-p-q-1))/(ss_tot/(n-1)) #R2 ajusté
  return(adj_r2)
}

adj_r2(sarima215)
ggseasonplot(tsALL_2013_2019, polar = FALSE)


