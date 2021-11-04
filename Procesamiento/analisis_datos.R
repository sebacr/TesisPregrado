pacman::p_load(dplyr, sjmisc, car, sjlabelled,labelled, stargazer,kableExtra,corrplot,sessioninfo,readxl,
               pander,xtable, tidyverse, extrafont, ggplot2, forcats, ggpubr, naniar,haven, devtools,summarytools,
               poLCA,sjPlot,lavaan)
install.packages("webshot",dependencies = TRUE)
library(webshot)
webshot::install_phantomjs(force = TRUE)

load("Input/Data_proc/data.RData") 




# Modelos de regresion con variables observables (sin latentes)






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









# SEM

# Regresión estructural

#model6c (manual specification) 
m6cc <- '
# measurement model (latent)
  
  AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
  perc_merit=~merit_perc_effort+merit_perc_talent

    
#single indicator factor
  
  edadf =~ 1*edad
  
#variance of exogenous (if dep observable=0) variable to 0
  
    edad ~~ 0*edad
    
# cov
  
  atrib_pob_1 ~~ atrib_pob_3

# regressions
 
  AI ~ perc_merit + edadf
'
fit6cc <- sem(m6cc, data=data,optim.method=list("BFGS"))
summary(fit6cc)


























#model6c (manual specification) 
m6cc <- '
# measurement model (latent)
  
  perc_merit=~merit_perc_effort+merit_perc_talent

    
#single indicator factor
  
  edadf =~ 1*edad
  atrib_pob_1f =~ 1*atrib_pob_1
  
#variance of exogenous (if dep observable=0) variable to 0
  
  edad ~~ 0*edad
  atrib_pob_1 ~~ 0*atrib_pob_1
    
# cov


# regressions
 
  atrib_pob_1f ~ perc_merit + edadf
'
fit6cc <- sem(m6cc, data=data,optim.method=list("BFGS"))
summary(fit6cc)


# Regresion multiple con lavaan


m2 <- '
  # regressions
    atrib_pob_1 ~ 1 + merit_perc_effort + merit_perc_talent + edad
 # covariance
    merit_perc_effort ~~ merit_perc_effort + edad
    merit_perc_talent ~~ edad
'
fit2 <- sem(m2, data=data)
summary(fit2)




# Modelo atribuciones riqueza: Trabajo duro
model_merit<-lm(atrib_pob_1 ~ merit_perc_effort+merit_perc_talent+edad, data=data)

sjPlot::tab_model(list(model_merit), show.ci=FALSE, p.style = "stars", 
                  dv.labels = c("Modelo 1"),string.pred = "Predictores", 
                  string.est = "β")
