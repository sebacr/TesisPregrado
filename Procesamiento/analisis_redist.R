pacman::p_load(dplyr, sjmisc, car, sjlabelled,labelled, stargazer,kableExtra,corrplot,sessioninfo,readxl,
               pander,xtable, tidyverse, extrafont, ggplot2, forcats, ggpubr, naniar,haven, devtools,summarytools,
               poLCA,sjPlot,lavaan)
#install.packages("webshot",dependencies = TRUE)
library(webshot)
webshot::install_phantomjs(force = TRUE)

load("Input/Data_proc/data_redist.RData") # Data redistribucion 


# Asociacion variables
data_cor <- data_redist %>% dplyr::select(merit_perc_effort,merit_perc_talent,merit_pref_effort,merit_pref_talent,
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


# Asociacion atribuciones
at_cor <- data_redist %>% dplyr::select(atrib_pob_1,atrib_pob_3,atrib_pob_4,atrib_pob_5,
                                          atrib_riq_1,atrib_riq_3,atrib_riq_4,atrib_riq_5)
coratrib=cor(at_cor, use = "complete.obs")
windowsFonts(A = windowsFont("Times New Roman"))
rownames(coratrib) <-c(
  "(1) Atribucion pobreza Falta habilidad",
  "(2) Atribucion pobreza Falta esfuerzo",
  "(3) Atribucion pobreza Sistema económico",
  "(4) Atribucion pobreza Sistema educativo",
  "(5) Atribucion riqueza Talento",
  "(6) Atribucion riqueza Trabajo duro",
  "(7) Atribucion riqueza Sistema económico",
  "(8) Atribucion riqueza Sistema educativo")
colnames(coratrib) <-c("(1)", "(2)","(3)","(4)","(5)", "(6)","(7)","(8)")
corrplot(coratrib,
         method = "color",
         type = "upper",
         tl.col = "black",
         addCoef.col = "black",
         diag = TRUE,
         family = "A",
         number.font = 6,
         tl.cex =0.75,
         number.cex = 1)





# Modelos de regresión


# Modelo atribuciones pobreza: Falta esfuerzo
m3_1<-lm(atrib_pob_3 ~ merit_perc_effort+merit_perc_talent, data=data_redist)
m3_2<-lm(atrib_pob_3 ~ merit_pref_effort+merit_pref_talent, data=data_redist)
m3_3<-lm(atrib_pob_3 ~ merit_perc_effort+merit_perc_talent+merit_pref_effort+merit_pref_talent, data=data_redist)
m3_4<-lm(atrib_pob_3 ~ merit_perc_effort+merit_perc_talent+merit_pref_effort+merit_pref_talent+sexo+edad+educ+ingresos+ess_ind, data=data_redist)

sjPlot::tab_model(list(m3_1, m3_2,m3_3,m3_4), show.ci=FALSE, p.style = "stars", 
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"),string.pred = "Predictores")



# Modelo preferencias redistributivas
m4_1<-lm(redist ~ merit_perc_effort+merit_perc_talent, data=data_redist)
m4_2<-lm(redist ~ merit_pref_effort+merit_pref_talent, data=data_redist)
m4_3<-lm(redist ~ atrib_pob_1+atrib_pob_3+merit_perc_effort+merit_perc_talent+merit_pref_effort+merit_pref_talent, data=data_redist)
m4_4<-lm(redist ~ merit_perc_effort+merit_perc_talent+merit_pref_effort+merit_pref_talent+sexo+edad+educ+ingresos+ess_ind, data=data_redist)
m4_5<-lm(redist ~ atrib_pob_1+atrib_pob_3+atrib_pob_4+atrib_pob_5+atrib_riq_1+atrib_riq_3+atrib_riq_4+atrib_riq_5+
           merit_perc_effort+merit_perc_talent+merit_pref_effort+merit_pref_talent+sexo+edad+educ+ingresos+ess_ind, data=data_redist)
m4_6<-lm(redist ~ atrib_riq_1+atrib_riq_3+atrib_pob_1+atrib_pob_3+merit_perc_effort+merit_perc_talent+merit_pref_effort+merit_pref_talent+sexo+edad+educ+ingresos+ess_ind, data=data_redist)

sjPlot::tab_model(list(m4_1, m4_2,m4_3,m4_4,m4_5), show.ci=FALSE, p.style = "stars", 
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4","Modelo 5"),string.pred = "Predictores")
