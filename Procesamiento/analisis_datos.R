pacman::p_load(dplyr, sjmisc, car, sjlabelled,labelled, stargazer,kableExtra,corrplot,sessioninfo,readxl,
               pander,xtable, tidyverse, extrafont, ggplot2, forcats, ggpubr, naniar,haven, devtools,summarytools,
               poLCA,sjPlot,lavaan)
install.packages("webshot",dependencies = TRUE)
library(webshot)
webshot::install_phantomjs(force = TRUE)

data_original <- read_spss("Input/factorial_ola2.sav")
load("Input/Data_proc/data.RData") 
# Asociacion variables
data_cor <- data %>% dplyr::select(merit_perc_effort,merit_perc_talent,merit_pref_effort,merit_pref_talent,
                                              atrib_pob_1,atrib_pob_3,
                                              atrib_riq_1,atrib_riq_3)
cormerit_fondecyt=cor(data_cor, use = "complete.obs")
windowsFonts(A = windowsFont("Times New Roman"))
rownames(cormerit_fondecyt) <-c(
  "(1) Percepción meritocratica basado en esfuerzo",
  "(2) Percepción meritocratica basado en talento",
  "(3) Preferencia meritocratica basado en esfuerzo",
  "(4) Preferencia meritocratica basado en talento",
  "(5) Atribucion pobreza Falta habilidad",
  "(6) Atribucion pobreza Falta esfuerzo",
  "(7) Atribucion riqueza Talento",
  "(8) Atribucion riqueza Trabajo duro")
colnames(cormerit_fondecyt) <-c("(1)", "(2)","(3)","(4)","(5)", "(6)","(7)","(8)")
corrplot(cormerit_fondecyt,
         method = "color",
         type = "upper",
         tl.col = "black",
         addCoef.col = "black",
         diag = TRUE,
         family = "A",
         number.font = 6,
         tl.cex =0.75,
         number.cex = 1)


at_pob <- data %>% dplyr::select(atrib_pob_1,atrib_pob_2,atrib_pob_3,atrib_pob_4,atrib_pob_5)
at_riq <- data %>% dplyr::select(atrib_riq_1,atrib_riq_2,atrib_riq_3,atrib_riq_4,atrib_riq_5)
percep_merit <- data %>% dplyr::select(merit_perc_effort,merit_perc_talent,merit_perc_wpart,merit_perc_netw)
pref_merit <- data %>% dplyr::select(merit_pref_effort,merit_pref_talent,merit_pref_wpart,merit_pref_netw)


# Factor analisis WLSMV

## Atribuciones internas pobreza

cfa.pob.int <- '
pobint =~ NA*atrib_riq_1+atrib_riq_2+atrib_riq_3+atrib_riq_4+atrib_riq_5
pobint ~~ 1*pobint
'

pob_int_mlsmv <-
  cfa(model = cfa.pob.int, data = data,
      mimic = "Mplus", estimator = "WLSMV",
      ordered = c("atrib_riq_1", "atrib_riq_2", "atrib_riq_3", "atrib_riq_4", "atrib_riq_5"))

lavaan::summary(pob_int_mlsmv, fit.measures = TRUE, standardized = TRUE)








# Modelos de regresion

# Modelo atribuciones pobreza: Falta habilidad
mp1_percep_merit<-lm(atrib_pob_1 ~ merit_perc_effort+merit_perc_talent, data=data)
mp1_pref_merit<-lm(atrib_pob_1 ~ merit_pref_effort+merit_pref_talent, data=data)
mp1_merit<-lm(atrib_pob_1 ~ merit_perc_effort+merit_perc_talent+merit_pref_effort+merit_pref_talent, data=data)
mp1_sociodemo<-lm(atrib_pob_1 ~ sexo+edad+educ+ingresos+ess_ind, data=data)
mp1_total<-lm(atrib_pob_1 ~ merit_perc_effort+merit_perc_talent+merit_pref_effort+merit_pref_talent+
                sexo+edad+educ+ingresos+ess_ind, data=data)

sjPlot::tab_model(list(mp1_percep_merit,mp1_pref_merit,mp1_merit,mp1_sociodemo,mp1_total), show.ci=FALSE, p.style = "stars", 
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5"),string.pred = "Predictores", 
                  string.est = "β", file = "Output/reg1_tab.html")
webshot::webshot("Output/reg1_tab.html","Output/reg1_tab.png")


# Modelo atribuciones pobreza: Falta esfuerzo
mp3_percep_merit<-lm(atrib_pob_3 ~ merit_perc_effort+merit_perc_talent, data=data)
mp3_pref_merit<-lm(atrib_pob_3 ~ merit_pref_effort+merit_pref_talent, data=data)
mp3_merit<-lm(atrib_pob_3 ~ merit_perc_effort+merit_perc_talent+merit_pref_effort+merit_pref_talent, data=data)
mp3_sociodemo<-lm(atrib_pob_3 ~ sexo+edad+educ+ingresos+ess_ind, data=data)
mp3_total<-lm(atrib_pob_3 ~ merit_perc_effort+merit_perc_talent+merit_pref_effort+merit_pref_talent+sexo+edad+educ+ingresos+ess_ind, data=data)

sjPlot::tab_model(list(mp3_percep_merit, mp3_pref_merit,mp3_merit,mp3_sociodemo,mp3_total), show.ci=FALSE, p.style = "stars", 
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5"),string.pred = "Predictores",
                  string.est = "β", file = "Output/reg2_tab.html")
webshot::webshot("Output/reg2_tab.html","Output/reg2_tab.png")


# Modelo atribuciones riqueza: Talento
mr1_percep_merit<-lm(atrib_riq_1 ~ merit_perc_effort+merit_perc_talent, data=data)
mr1_pref_merit<-lm(atrib_riq_1 ~ merit_pref_effort+merit_pref_talent, data=data)
mr1_merit<-lm(atrib_riq_1 ~ merit_perc_effort+merit_perc_talent+merit_pref_effort+merit_pref_talent, data=data)
mr1_total<-lm(atrib_riq_1 ~ merit_perc_effort+merit_perc_talent+merit_pref_effort+merit_pref_talent+sexo+edad+educ+ingresos+ess_ind, data=data)

sjPlot::tab_model(list(mr1_percep_merit, mr1_pref_merit,mr1_merit,mr1_total), 
                  show.ci=FALSE, p.style = "stars", dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"),
                  string.pred = "Predictores", string.est = "β", file = "Output/reg3_tab.html")
webshot::webshot("Output/reg3_tab.html","Output/reg3_tab.png")


# Modelo atribuciones riqueza: Trabajo duro
mr3_percep_merit<-lm(atrib_riq_3 ~ merit_perc_effort+merit_perc_talent, data=data)
mr3_pref_merit<-lm(atrib_riq_3 ~ merit_pref_effort+merit_pref_talent, data=data)
mr3_merit<-lm(atrib_riq_3 ~ merit_perc_effort+merit_perc_talent+merit_pref_effort+merit_pref_talent, data=data)
mr3_total<-lm(atrib_riq_3 ~ merit_perc_effort+merit_perc_talent+merit_pref_effort+merit_pref_talent+sexo+edad+educ+ingresos+ess_ind, data=data)

sjPlot::tab_model(list(mr3_percep_merit, mr3_pref_merit,mr3_merit,mr3_total), show.ci=FALSE, p.style = "stars", 
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"),string.pred = "Predictores", 
                  string.est = "β", file = "Output/reg4_tab.html")
webshot::webshot("Output/reg4_tab.html","Output/reg4_tab.png")
