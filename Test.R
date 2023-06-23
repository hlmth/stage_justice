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
library(CausalImpact) #pour les effets causaux (intervention analysis)

mens_aggreg <- read_sas("C:/Users/hugo.lamothe/Desktop/HUGO/Donnees/mens_agreg.sas7bdat")


View(mens_aggreg)


mens_aggreg_ <- mens_aggreg %>% 
  group_by(dt_mois, quartier_etab) %>% summarise(detenus = sum(detenus), .groups = "keep")

View(mens_aggreg_)



MA <- mens_aggreg %>% filter(quartier_etab == "MA/QMA") %>% group_by(dt_mois) %>% summarise(detenus_MA = sum(detenus) )
View(MA)                             
MA_2013 <- MA[107:190,]

tsma <- ts(MA$detenus_MA, start = 2004, frequency = 12)
tsma_2013 <- ts(MA_2013$detenus_MA, start = 2013, frequency = 12)
plot(tsma_2013)


plot(tsma,
     xlab = "Année")
acf(tsma)
pacf(tsma)
monthplot(tsma)
lag.plot (tsma , lags =12 , layout =c(3 ,4) ,do.lines = FALSE )


plot(decompose(tsma))



summary(lm(tsma ~ seq(1, 231))) #on teste la présence d'une tendance

#même chose avec un test de Dickey Fuller
test.df.trend <- ur.df(log(tsma),
                       type = "trend", lags = 0)

res.df <- data.frame(as.vector(test.df.trend@teststat),
                     test.df.trend@cval)
names(res.df) <- c("Stat","CV 1pct", "CV 5pct", "CV 10pct")
xtable(res.df) %>%
  kable(digits=2) %>%
  kable_styling()
#On vérifie que la tendance est bien déterministe
test.df.trend <- ur.df(diff(log(tsma)),
                       type = "drift", lags = 1)

res.df <- data.frame(as.vector(test.df.trend@teststat),
                     test.df.trend@cval)
names(res.df) <- c("Stat","CV 1pct", "CV 5pct", "CV 10pct")
xtable(res.df) %>%
  kable(digits=2) %>%
  kable_styling()

#Fonction utile pour plus tard
modesspectral <- function(spec)
{# L'argument spec est le résultat de la fonction parzen.wge
x <- spec[[2]]
n <- length(x)
aux1 <- x[2:(n-1)] - x[1:(n-2)]
aux2 <- x[2:(n-1)] - x[3:n]
aux <- (aux1>0)*(aux2>0)
specmodes <- which(aux >0)
freq <- spec[[1]][specmodes]
res <- rbind(freq,spec[[2]][specmodes],1/freq)
rownames(res) <- c("Fréquences","Densité","Pèriodes")
res
}


diffp <- diff(log(tsma), differences = 1)
plot(diffp)

diffs <- diff(log(tsma), differencies = 1, lag =12)
plot(diffs)

diffsetp <- diff(diffp, differences = 1, lag = 12)
plot(diffsetp)


spectral <- parzen.wge(diffp)
modesspectral(spectral) #on a de la périodicité principalement tous les ans et tous les semestres 

summary(lm(diffp ~ seq(1, 230))) #on a bien plus de tendance ni de constante
summary(lm(diffs ~ seq(1, 219))) #là non plus
summary(lm(diffsetp ~ seq(1, 218))) #là non plus

plot(decompose(diffp))
plot(decompose(diffs))
plot(decompose(diffsetp))
#on observe si la série différencée pourrait sembler stationnaire
#on voit bien que la série diffs ne parrait pas du tout stationnaire, 
#la saisonnalité ne permettant pas d'expliquer toute la série

adf.test(diffp)
kpss.test(diffp)
pp.test(diffp)
#
adf.test(diffs)
kpss.test(diffs)
pp.test(diffs) #on rejette l'hypothèse nulle à 5% contre 1% dans les autres cas
#
adf.test(diffsetp)
kpss.test(diffsetp)
pp.test(diffsetp)


# 
# adfTest_valid <- function(series,kmax,type){ #test adf jusqu'à des résidus non auto-corrélés 
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



acf(as.numeric(diffp), main = 'ACF de la série différenciée p', lag.max = 50)
pacf(as.numeric(diffp), main= 'PACF de la série différenciée', lag.max = 50)
acf2(diffp) #c'est saisonnié 

acf(as.numeric(diffs), main = 'ACF de la série différenciée s', lag.max = 50)
pacf(as.numeric(diffs), main= 'PACF de la série différenciée s', lag.max = 50)
acf2(diffs) #c'est non saisonnié

acf(as.numeric(diffsetp), main = 'ACF de la série différenciée s et p', lag.max = 50)
pacf(as.numeric(diffsetp), main= 'PACF de la série différenciée s et p', lag.max = 50)
acf2(diffsetp) #c'est saisonnié


table <- data.frame(model = character(),AIC=  numeric(),BIC = numeric())
for (i in c(0:13)){
  for (j in c(0:24)){
    model <- arima(diffp, order  = c(i,0,j))
    aic <- round(AIC(model),3)
    bic <- round(BIC(model),3)
    table[nrow(table) +1, ] <- c(paste0('arima(',
                                        as.character(i),',0,',as.character(j),')'), AIC = aic, BIC = bic)
  }
}

print(table) #On affiches l'AIC et le BIC pour chaque modÃ¨le

print( table[which.min(table$AIC),]) 
print( table[which.min(table$BIC),]) 

arima215 <- Arima(tsma, order = c(2,1,5), include.mean = F)
sarima215 <- Arima(tsma, order = c(2,1,5), seasonal = c(2,0,0), include.mean = F)

Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l,
                                           type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}


Qtests(arima215$residuals, 24, fitdf = 5) 
#Rien ne rejette l'abscence d'autocorrÃ©lation des rÃ©sidus, le modÃ¨le est valide.

# 
adj_r2 <- function(model){
  ss_res <- sum(model$residuals^2) #somme des rÃ©sidus au carrÃ©
  p <- model$arma[1] 

  q <- model$arma[2]
  ss_tot <- sum(diffp[-c(1:max(p,q))]^2)
  n <- model$nobs-max(p,q) 
  adj_r2 <- 1-(ss_res/(n-p-q-1))/(ss_tot/(n-1)) #R2 ajustÃ©
  return(adj_r2)
}

adj_r2(sarima215)
ggseasonplot(tsma_2013, polar = FALSE)

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


