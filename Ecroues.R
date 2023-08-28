mens_aggreg <- read_sas("~/work/mens_agreg.sas7bdat")


ALL <- mens_aggreg %>%
  filter(year(dt_mois) >= 2016) %>%
  group_by(dt_mois) %>%
  summarise(ecroues = sum(ecroues), ecroues_non_det = sum(ecroues) - sum(detenus))

ecroues <- ts(ALL$ecroues, start = c(year(min(ALL$dt_mois)), month(min(ALL$dt_mois))), frequency = 12)
ecroues_non_det <- ts(ALL$ecroues_non_det, start = c(year(min(ALL$dt_mois)), month(min(ALL$dt_mois))), frequency = 12)
var <- NULL
for (i in c(1:length(temps))){
  var <- c(var,TC(0.9, temps[[i]], 53))
}
var <- ts(var, start = start(detenus), frequency = 12) #On créé le régresseur TC avec la valeur du alpha déterminée précédement qui sera complété avec un outliers AO


spec_TCAO <- x13_spec(spec = "RSA5c",
                      tradingdays.option = "None",
                      usrdef.outliersEnabled = TRUE,
                      usrdef.outliersType = "AO",
                      usrdef.outliersDate = "2020-04-01",
                      usrdef.varEnabled = TRUE,
                      usrdef.var = var,
                      usrdef.varType = "Irregular",
                      transform.function = "Auto",
                      easter.enabled = FALSE,
                      outlier.enabled = FALSE)

mod_ecrou <- x13(ecroues, spec_TCAO)
mod_ecrou_non_det <- x13(ecroues_non_det, spec_TCAO)

summary(mod_ecrou$regarima)
plot(mod_ecrou, type_chart = "sa-trend")
plot(mod_ecrou)
plot(mod_ecrou$decomposition)
layout(matrix(1:6, 3, 2));plot(mod_ecrou$regarima, ask = FALSE);layout(matrix(1))
dev.off()

summary(mod_ecrou_non_det$regarima)
plot(mod_ecrou_non_det, type_chart = "sa-trend")
plot(mod_ecrou_non_det)
plot(mod_ecrou_non_det$decomposition)
layout(matrix(1:6, 3, 2));plot(mod_ecrou_non_det$regarima, ask = FALSE);layout(matrix(1))
dev.off()


