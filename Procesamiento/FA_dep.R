pacman::p_load(dplyr, car, sjlabelled,labelled, stargazer,kableExtra,corrplot,
               tidyverse, ggplot2, ggpubr,haven, devtools,summarytools,nortest,tseries,psych,
               sjPlot,lavaan,semPlot,pander,standarize,texreg)
source('https://raw.githubusercontent.com/brandmaier/onyxR/master/tools/install.R')
library(onyxR)
#install.packages("webshot",dependencies = TRUE)
library(webshot)
webshot::install_phantomjs(force = TRUE)
load("Input/Data_proc/data.RData") # Data 

atribcor <- data %>% dplyr::select(atrib_pob_1,atrib_pob_2,atrib_pob_3,atrib_pob_4,atrib_pob_5,
                                  atrib_riq_1,atrib_riq_2,atrib_riq_3,atrib_riq_4,atrib_riq_5) # Var deps

names(atribcor)
atribcor %>% 
  psych::describe() %>% 
  as.data.frame() %>% 
  dplyr::select("Mean"=mean,"SD"=sd,"Min"=min,"Max"=max) %>% 
  round(2) ->desc.issp
desc.issp

# Correlación de pearson

pearson_atrib=cor(atribcor, use = "complete.obs")
windowsFonts(A = windowsFont("Times New Roman"))
rownames(pearson_atrib) <-c(
  "(1) Atribuciones pobreza Falta habilidad",
  "(2) Atribuciones pobreza Mala suerte",
  "(3) Atribuciones pobreza Falta esfuerzo",
  "(4) Atribuciones pobreza Sistema económico",
  "(5) Atribuciones pobreza Sistema educativo",
  "(6) Atribuciones riqueza Talento",
  "(7) Atribuciones riqueza Suerte",
  "(8) Atribuciones riqueza Trabajo duro",
  "(9) Atribuciones riqueza Sistema económico",
  "(10) Atribuciones riqueza Sistema educativo")
colnames(pearson_atrib) <-c("(1)", "(2)","(3)","(4)","(5)", "(6)","(7)","(8)","(9)","(10)")
corrplot(pearson_atrib,
         method = "color",
         type = "upper",
         tl.col = "black",
         addCoef.col = "black",
         diag = TRUE,
         family = "A",
         number.font = 6,
         tl.cex =0.75,
         number.cex = 1)

# Correlación policlorica 

polycor_atrib <- psych::polychoric(atribcor)
windowsFonts(A = windowsFont("Times New Roman"))
rownames(polycor_atrib$rho) <-c(
  "(1) Atribuciones pobreza Falta habilidad",
  "(2) Atribuciones pobreza Mala suerte",
  "(3) Atribuciones pobreza Falta esfuerzo",
  "(4) Atribuciones pobreza Sistema económico",
  "(5) Atribuciones pobreza Sistema educativo",
  "(6) Atribuciones riqueza Talento",
  "(7) Atribuciones riqueza Suerte",
  "(8) Atribuciones riqueza Trabajo duro",
  "(9) Atribuciones riqueza Sistema económico",
  "(10) Atribuciones riqueza Sistema educativo"
)
colnames(polycor_atrib$rho) <-c("(1)", "(2)","(3)","(4)","(5)", "(6)","(7)","(8)","(9)","(10)")
corrplot(polycor_atrib$rho,
         method = "color",
         type = "upper",
         tl.col = "black",
         addCoef.col = "black",
         diag = TRUE,
         number.font = 6,
         tl.cex =0.75,
         number.cex = 1)

# Observar distribución de respuestas en variables dependientes

data %>%
  pivot_longer(atrib_pob_1:atrib_riq_5, names_to = "question", values_to = "response") %>%
  ggplot(aes(x = response)) +
  geom_bar() +
  facet_wrap(vars(question), ncol = 5) +
  labs(x = "Response (on a 1 to 5 scale)", y = "Number of respondents")












# EFA

# Primero chequear normalidad en variables dependientes

# Create correlation matrix
dep_fa1 <- data %>% dplyr::select(atrib_pob_1,atrib_pob_2,atrib_pob_3,atrib_pob_4,atrib_pob_5,
                            atrib_riq_1,atrib_riq_2,atrib_riq_3,atrib_riq_4,atrib_riq_5) # Var deps

dep_fa2 <- data %>% dplyr::select(atrib_pob_1,atrib_pob_3,atrib_pob_4,atrib_pob_5,
                                 atrib_riq_1,atrib_riq_3,atrib_riq_4,atrib_riq_5) # Var deps


cor_dep_fa1 =cor(dep_fa1, use = "complete.obs")
cor_dep_fa2 =cor(dep_fa2, use = "complete.obs")
# Cronbach Aplha-s
#psych::alpha(cor_control, keys=NULL,cumulative=FALSE, title=NULL,na.rm = TRUE,
#             check.keys=TRUE,n.iter=1,delete=TRUE)
#psych::alpha(cor_cambio, keys=NULL,cumulative=FALSE, title=NULL,na.rm = TRUE,
#             check.keys=TRUE,n.iter=1,delete=TRUE)
#psych::alpha(cor_castigo, keys=NULL,cumulative=FALSE, title=NULL,na.rm = TRUE,
#             check.keys=TRUE,n.iter=1,delete=TRUE)
#psych::alpha(cor_dep_fa1, keys=NULL,cumulative=FALSE, title=NULL,na.rm = TRUE,
#             check.keys=TRUE,n.iter=1,delete=TRUE)
## Normality test
lillie.test(dep_fa1$atrib_pob_1)
lillie.test(dep_fa1$atrib_pob_2)
lillie.test(dep_fa1$atrib_pob_3)
lillie.test(dep_fa1$atrib_pob_4)
lillie.test(dep_fa1$atrib_pob_5)
lillie.test(dep_fa1$atrib_riq_1)
lillie.test(dep_fa1$atrib_riq_2)
lillie.test(dep_fa1$atrib_riq_3)
lillie.test(dep_fa1$atrib_riq_4)
lillie.test(dep_fa1$atrib_riq_5)
jarque.bera.test(na.omit(dep_fa1$atrib_pob_1))
jarque.bera.test(na.omit(dep_fa1$atrib_pob_2))
jarque.bera.test(na.omit(dep_fa1$atrib_pob_3))
jarque.bera.test(na.omit(dep_fa1$atrib_pob_4))
jarque.bera.test(na.omit(dep_fa1$atrib_pob_5))
jarque.bera.test(na.omit(dep_fa1$atrib_riq_1))
jarque.bera.test(na.omit(dep_fa1$atrib_riq_2))
jarque.bera.test(na.omit(dep_fa1$atrib_riq_3))
jarque.bera.test(na.omit(dep_fa1$atrib_riq_4))
jarque.bera.test(na.omit(dep_fa1$atrib_riq_5))
## Determinant
det(cor_dep_fa1) # Resultado: 0.1188577 This value is greater than the necessary value of 0.00001 (see section 17.5). As such, our determinant does not seem problematic.
det(cor_dep_fa2) # Resultado: 0.1641296 This value is greater than the necessary value of 0.00001 (see section 17.5). As such, our determinant does not seem problematic.

## KMO
KMO(dep_fa1)
KMO(dep_fa2)

# Gráfico de sedimentacion

scree(cor_dep_fa1)
scree(cor_dep_fa2)





# EFA by PA

#Modelo 1 factor
fa_pa.1<- fa(r=dep_fa1, nfactors = 1, rotate = "oblimin", fm="pa")
fa_ml.1<- fa(r=dep_fa1, nfactors = 1, rotate = "oblimin", fm="ml")
colnames(fa_pa.1$loadings) <- c("AT") #Cambio etiqueta factores
#rownames(fa_pa.1$loadings) <- c("Carab. Represión", "Carab. Desalojo", "Est. Piedras.", "Dano Inmobiliario.", "Dano Transporte.", "Dano Locales") # Cambio etiqueta indicadores
fa.sort(fa_ml.1)
fa.sort(fa_pa.1) # Resultados
fa.diagram(fa_pa.1) # Diagrama





# Modelo 2 factores con indicadores fatalistas
fa_pa.2_fatal<- fa(r=dep_fa1, nfactors = 2, rotate = "oblimin", fm="pa")
fa_ml.2_fatal<- fa(r=dep_fa1, nfactors = 2, rotate = "oblimin", fm="ml")
colnames(fa_pa.2_fatal$loadings) <- c("AT. Int", "AT. Ext")
colnames(fa_ml.2_fatal$loadings) <- c("AT. Int", "AT. Ext")
#rownames(fa_pa.2_fatal$loadings) <- c("Carab. Represión", "Carab. Desalojo", "Est. Piedras.", "Dano Inmobiliario.", "Dano Transporte.", "Dano Locales")
fa.sort(fa_pa.2_fatal)
fa.sort(fa_ml.2_fatal)
fa.diagram(fa_pa.2_fatal)

# Modelo 2 factores sin fatalismo
fa_pa.2<- fa(r=dep_fa2, nfactors = 2, rotate = "oblimin", fm="pa")
colnames(fa_pa.2$loadings) <- c("AT. Int", "AT. Ext")
#rownames(fa_pa.2$loadings) <- c("Carab. Represión", "Carab. Desalojo", "Est. Piedras.", "Dano Inmobiliario.", "Dano Transporte.", "Dano Locales")
fa.sort(fa_pa.2)
fa.diagram(fa_pa.2)




#Modelo 3 factores
fa_pa.3<- fa(r=dep_fa1, nfactors = 3, rotate = "oblimin", fm="pa")
colnames(fa_pa.3$loadings) <- c("AT. Int", "AT. Ext", "AT. Fatalis")
#rownames(fa_pa.3$loadings) <- c("Carab. Represión", "Carab. Desalojo", "Est. Piedras.", "Dano Inmobiliario.", "Dano Transporte.", "Dano Locales")
fa.sort(fa_pa.3)
fa.diagram(fa_pa.3)





#Modelo 4 factores
fa_pa.4<- fa(r=dep_fa1, nfactors = 4, rotate = "oblimin", fm="pa") # maximum iteration exceeded












# CFA


# Modelo con 2 factores incluyendo fatalismo


m2<-'
AI=~atrib_pob_1+atrib_pob_2+atrib_pob_3+atrib_riq_1+atrib_riq_2+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
'

m2.fit<-cfa(m2,data=data,ordered=T)
summary(m2.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(m2.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))


resid(m2.fit, type = "cor")


# Modificación de indices
modindices(m2.fit,sort. = T, minimum.value = 10.82)



## Crear modelos revisados 

m2_2<-'
AI=~atrib_pob_1+atrib_pob_2+atrib_pob_3+atrib_riq_1+atrib_riq_2+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
atrib_pob_2 ~~ atrib_riq_2
'

m2_2.fit<-cfa(m2_2,data=data,ordered=T)
summary(m2_2.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)


resid(m2_2.fit, type = "cor")

# Modificación de indices

modindices(m2_2.fit,sort. = T, minimum.value = 10.82)



m2_3<-'
AI=~atrib_pob_1+atrib_pob_2+atrib_pob_3+atrib_riq_1+atrib_riq_2+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4
atrib_pob_2 ~~ atrib_riq_2
'

m2_3.fit<-cfa(m2_3,data=data,ordered=T)
summary(m2_3.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)     
semPaths(m2_3.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

resid(m2_3.fit, type = "cor")

# Modificación de indices

modindices(m2_3.fit,sort. = T, minimum.value = 10.82)






m2_4<-'
AI=~atrib_pob_1+atrib_pob_2+atrib_pob_3+atrib_riq_1+atrib_riq_2+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4

# Cov
atrib_pob_2 ~~ atrib_riq_2
atrib_riq_1 ~~ atrib_riq_3
'

m2_4.fit<-cfa(m2_4,data=data,ordered=T)
summary(m2_4.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T) # # # # # # Mejor modelo
semPaths(m2_4.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

save(m2_4.fit,file = "Output/Tablas/m2_4.fit.RData")












# Modelo con 3 factores 


m3<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
AF=~atrib_pob_2+atrib_riq_2
'

m3.fit<-cfa(m3,data=data,estimator="WLSMV",ordered=T)
summary(m3.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(m3.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

fitMeasures(m3.fit)

# Ver residuos 

resid(m3.fit, type = "cor")

# Modificación de indices
modindices(m3.fit,sort. = T, minimum.value = 10.82)



m3_2<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4
AF=~atrib_pob_2+atrib_riq_2
'

m3_2.fit<-cfa(m3_2,data=data,estimator="WLSMV",ordered=T)
summary(m3_2.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(m3_2.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

# Modificación de indices
modindices(m3_2.fit,sort. = T, minimum.value = 10.82)



m3_3<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4
AF=~atrib_pob_2+atrib_riq_2

# Correlaciones
atrib_riq_1 ~~ atrib_riq_3
'

m3_3.fit<-cfa(m3_3,data=data,ordered=T)
summary(m3_3.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)  #  #  #  #  Mejor modelo
semPaths(m3_3.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

# Modificación de indices

modindices(m3_3.fit,sort. = T, minimum.value = 10.82)

save(m3_3.fit,file = "Output/Tablas/m3_3.fit.RData")




m3_sin_atr5.fit<-cfa(m3_sin_atr5,data=data,estimator="WLSMV",ordered=T)
summary(m3_sin_atr5.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(m3_sin_atr5.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))







# Modelo de dos factores sin indicadores de fatalismo

msf<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
'

msf.fit<-cfa(msf,data=data,estimator="WLSMV",ordered=T)
summary(msf.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(msf.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

resid(msf.fit, type = "cor")

# Modificación de indices
modindices(msf.fit,sort. = T, minimum.value = 10.82)



msf_2<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4
'

msf_2.fit<-cfa(msf_2,data=data,estimator="WLSMV",ordered=T)
summary(msf_2.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

resid(msf_2.fit, type = "cor")

# Modificación de indices

modindices(msf_2.fit,sort. = T, minimum.value = 10.82)



msf_3<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4

# Cov
atrib_riq_1 ~~ atrib_riq_3
'

msf_3.fit<-cfa(msf_3,data=data,estimator="WLSMV",ordered=T)
summary(msf_3.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T) #  #  #  #  Mejor modelo
semPaths(msf_3.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

save(msf_3.fit,file = "Output/Tablas/msf_3.fit.RData")

#onyx(msf_3.fit)



# Modelo con factores de segundo orden


m_so<-'
AI_pob=~atrib_pob_1+atrib_pob_2+atrib_pob_3
AE_pob=~atrib_pob_4+atrib_pob_5
AI_riq=~atrib_riq_1+atrib_riq_2+atrib_riq_3
AE_riq=~atrib_riq_4+atrib_riq_5
'

m_so.fit<-cfa(m_so,data=data,estimator="WLSMV",ordered=T)
#standardizedSolution(m2_fatal.fit)
summary(m_so.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(m_so.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

resid(m_so.fit, type = "cor")


# Modificación de indices
modindices(m_so.fit,sort. = T, minimum.value = 10.82)



## Crear modelos revisados 

m_so_2<-'
AI_pob=~atrib_pob_1+atrib_pob_2+atrib_pob_3
AE_pob=~atrib_pob_4+atrib_pob_5
AI_riq=~atrib_riq_1+atrib_riq_2+atrib_riq_3
AE_riq=~atrib_riq_4+atrib_riq_5

# Cov
atrib_pob_2 ~~ atrib_riq_2
'

m_so_2.fit<-cfa(m_so_2,data=data,estimator="WLSMV",ordered=T)
#standardizedSolution(m2_fatal.fit)
summary(m_so_2.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(m_so_2.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

resid(m_so_2.fit, type = "cor")


# Modificación de indices
modindices(m_so_2.fit,sort. = T, minimum.value = 10.82)



m_so_3<-'
AI_pob=~atrib_pob_1+atrib_pob_2+atrib_pob_3
AE_pob=~atrib_pob_4+atrib_pob_5
AI_riq=~atrib_riq_1+atrib_riq_2+atrib_riq_3
AE_riq=~atrib_riq_4+atrib_riq_5

# Cov
atrib_pob_2 ~~ atrib_riq_2
'

m_so_3.fit<-cfa(m_so_3,data=data,estimator="WLSMV",ordered=T)
#standardizedSolution(m2_fatal.fit)
summary(m_so_3.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(m_so_3.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

# Modificación de indices
modindices(m_so_3.fit,sort. = T, minimum.value = 10.82)




m_so_3<-'
AI_pob=~atrib_pob_1+atrib_pob_3
AE_pob=~atrib_pob_4+atrib_pob_5
AI_riq=~atrib_riq_1+atrib_riq_3
AE_riq=~atrib_riq_4+atrib_riq_5

# SO
AI =~ AI_pob+AI_riq
AE =~ AE_pob+AE_riq
'

m_so_3.fit<-cfa(m_so_3,data=data,estimator="WLSMV",ordered=T)
#standardizedSolution(m2_fatal.fit)
summary(m_so_3.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(m_so_3.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

# Modificación de indices
modindices(m_so_3.fit,sort. = T, minimum.value = 10.82)





m_so_final<-'
AI_pob=~atrib_pob_1+atrib_pob_2+atrib_pob_3
AE_pob=~atrib_pob_4+atrib_pob_5
AI_riq=~atrib_riq_1+atrib_riq_2+atrib_riq_3+atrib_riq_5
AE_riq=~atrib_riq_4+atrib_riq_5

# Cov
atrib_pob_2 ~~ atrib_riq_2

# SO
AI =~ AI_pob + AI_riq
AE =~ AE_pob + AE_riq
'

m_so_final.fit<-cfa(m_so_final,data=data,estimator="WLSMV",ordered=T)
#standardizedSolution(m2_fatal.fit)
summary(m_so_3.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(m_so_3.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))





# Segundo orden sin fatalismo


m_so_2<-'
AI_pob=~atrib_pob_1+atrib_pob_2+atrib_pob_3
AE_pob=~atrib_pob_4+atrib_pob_5
AI_riq=~atrib_riq_1+atrib_riq_2+atrib_riq_3
AE_riq=~atrib_riq_4+atrib_riq_5

# Cov
atrib_pob_2 ~~ atrib_riq_2
'

m_so_2.fit<-cfa(m_so_2,data=data,estimator="WLSMV",ordered=T)
#standardizedSolution(m2_fatal.fit)
summary(m_so_2.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(m_so_2.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

resid(m_so_2.fit, type = "cor")


# Modificación de indices
modindices(m_so_2.fit,sort. = T, minimum.value = 10.82)














msf_3<-'
Atribuciones_Internas=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
Atribuciones_Externas=~atrib_pob_4+atrib_pob_5+atrib_riq_4

# Cov
atrib_riq_1 ~~ atrib_riq_3
'
msf_3.fit<-cfa(msf_3,data=data,ordered=T)
dep_labs <- c(
  "Atribuciones pobreza Falta habilidad",
  "Atribuciones pobreza Falta esfuerzo",
  "Atribuciones riqueza Talento",
  "Atribuciones riqueza Trabajo duro",
  "Atribuciones pobreza Sistema económico",
  "Atribuciones pobreza Sistema educativo",
  "Atribuciones riqueza Sistema económico"
)
tb.load<- data.frame(round(cbind(lavaan::inspect(msf_3.fit,
                                                 what="std")$lambda),
                           digits = 2))
tb.load[tb.load==c(0.00)] <- NA

for (i in names(tb.load)) {
  # tb.load[,i] <- sjlabelled::as_character(tb.load[,i])
  tb.load[,i] <- sprintf(tb.load[,i], fmt = '%#.2f')
}
tb.load[tb.load=="NA"] <- ""
#-------#
fm01<- data.frame(t(data.frame(lavaan::fitmeasures(msf_3.fit, output ="matrix")[c("chisq","df","cfi","tli","rmsea"),]))); row.names(fm01) ="Atribuciones_Internas"

#------chi2, df------#
fm04<- round(rbind(fm01),3)
fm04.1 <- fm04 %>% dplyr::select(chisq,df) 
fm04.1$chisq <- round(x = fm04.1$chisq,digits = 1)
fm04.1$df <- round(x = fm04.1$df,digits = 0)
fm04.1$chi2df <- paste0(fm04.1$chisq,"(",fm04.1$df,")")
fm04.1 <- dplyr::select(fm04.1,"chi2df")
for (i in names(fm04.1)) {
  fm04.1[,i] <- as.character(fm04.1[,i])
}

#------CFI, RMSEA------#
fm04.2 <- fm04 %>% dplyr::select(cfi,tli,rmsea) 
for (i in names(fm04.2)) {
  fm04.2[,i] <- sprintf(fm04.2[,i], fmt = '%#.3f')
}

fm.df      <- dplyr::bind_cols(fm04.1,fm04.2)
fm.df$nobs <- c(lavaan::nobs(msf_3.fit)) 
fm.df <- data.frame(t(fm.df)); colnames(fm.df) <- c("Atribuciones_Internas")


#------ merge ------#
tb.fm<- dplyr::bind_rows(tb.load,fm.df)
tb.fm<- tb.fm %>% 
  dplyr::mutate(Variables=c(dep_labs,"$\\chi^2\\text{(df)}$","$\\text{CFI}$",
                       "$\\text{TLI}$","$\\text{RMSEA}$","$N$")) %>%
  dplyr::select(Variables,everything())
tb.atrib.fit.2f <- tb.fm

save(tb.atrib.fit.2f,file = "Output/Tablas/tb.atrib.fit.2f.RData")





# Comparación ajustes entre modelos

load("Output/Tablas/m2_4.fit.RData")
load("Output/Tablas/m3_3.fit.RData")
load("Output/Tablas/msf_3.fit.RData")

sum_fit<- bind_rows(fitmeasures(m2_4.fit)[c("chisq.scaled","df","cfi.scaled","rmsea","rmsea.scaled")],
                    fitmeasures(m3_3.fit)[c("chisq.scaled","df","cfi.scaled","rmsea","rmsea.scaled")],
                    fitmeasures(msf_3.fit)[c("chisq.scaled","df","cfi.scaled","rmsea","rmsea.scaled")])
sum_fit$mod <- c("Modelo 1","Modelo 2","Modelo 3")
sum_fit$est <- c("DWLS","DWLS","DWLS")
sum_fit <- select(sum_fit,mod,est,everything())
colnames <- c("Modelo","Estimador","$\\chi^2$","df","CFI.sca","RMSEA","RMSEA.sca")

sumtable02<- kable(sum_fit,digits = 3,format = "html",row.names = F,booktabs=T, caption = "Summary fit indices",col.names = colnames,escape = FALSE) %>%
  kable_styling(full_width = F)  %>%
  collapse_rows(columns = 1,valign = "middle")  %>%
  footnote(number = c("Modelo 1: dos factores incluyendo atribuciones fatalistas",
                      "Modelo 2: tres factores: internas, externas y fatalistas",
                      "Modelo 3: dos factores excluyendo atribuciones fatalistas"));sumtable02




# Path graph

msf_3<-'
Atribuciones_Internas=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
Atribuciones_Externas=~atrib_pob_4+atrib_pob_5+atrib_riq_4

# Cov
atrib_riq_1 ~~ atrib_riq_3
'
msf_3.fit<-cfa(msf_3,data=data,ordered=T)
summary(msf_3.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(msf_3.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

nodeNames <-c("Atribuciones pobreza Falta habilidad",
              "Atribuciones pobreza Falta esfuerzo",
              "Atribuciones riqueza Talento",
              "Atribuciones riqueza Trabajo duro",
              "Atribuciones pobreza Sistema económico",
              "Atribuciones pobreza Sistema educativo",
              "Atribuciones riqueza Sistema económico",
              "Atribuciones \n internas","Atribuciones \n externas")

# graphics.off()
msf_path<- data.frame(v1=fitmeasures(msf_3.fit, output ="matrix")[c("chisq.scaled","df","cfi.scaled","rmsea.scaled"),])
msf_path <- round(msf_path,3)
par(mai = c(2,2,2,2)) # Set the margin on all sides to 2
par(mar = c(5, 5, 5, 5)) # Set the margin on all sides to 6
layout(matrix(c(1, # semPlot
                1, # semPlot
                1,
                1,
                1,
                2),# ajuste
              nrow=6,
              byrow=TRUE))

layout.show(n = 2)
semPaths(
  msf_3.fit ,
  # whatLabels = "std", # Indica si aparecen los valores
  what = "mod",
  label.cex = 1.3, # Tamaño de las etiquetas dentro de los nodos.
  edge.label.cex =  0.8, #  0.8 Tamaño de los valores estimados.
  residuals = T,
  optimizeLatRes =F,
  edge.color = "black",
  style = "lisrel",
  nCharNodes = 0,
  curvePivot = F,
  curve = 2.5,
  rotation = 4,
  layout = "tree2",
  cardinal = "lat cov",
  legend.cex = 0.6, # 0.6
  label.cex = 1,
  label.font = 6,
  edge.label.font = 14,
  asize = 3,
  edge.width = 1.2,
  sizeMan = 8,# largo manifest
  sizeMan2= 8,# alto manifest
  sizeLat = 5.5, # largo de los circulos
  sizeLat2 = 20, # alto de los circulos
  residScale = 10,
  width = 21.5, # 17
  height = 10, # 10
  # nodeNames = nodeNames,
  intercepts = F,
  reorder = T,
  thresholds = F,
  fixedStyle =1,
  node.height = 1,
  node.width = 4,
  label.scale = F,
  shapeMan = "rectangle",
  shapeLat = "ellipse",
  nodeLabels = nodeNames,
  details = T)

ld<- standardizedsolution(msf_3.fit) %>% select(lhs,op,rhs,est.std) %>% filter(op=="=~") 
ld$est.std<- sprintf("%.2f", ld$est.std)

#pegar cargas factoriales al plot
text(x = -0.1,y =  0.88, ld$est.std[1],font = 5,cex = 1.20)
text(x = -0.1,y =  0.68, ld$est.std[2],font = 5,cex = 1.20)
text(x = -0.1,y =  0.50, ld$est.std[3],font = 5,cex = 1.20)
text(x = -0.1,y =  0.32, ld$est.std[4],font = 5,cex = 1.20)
text(x = -0.1,y = -0.38, ld$est.std[5],font = 5,cex = 1.20)
text(x = -0.1,y = -0.58, ld$est.std[6],font = 5,cex = 1.20)
text(x = -0.1,y = -0.75, ld$est.std[7],font = 5,cex = 1.20)
text(x = -1.75,y = 0.15, "0.39",font = 5,cex = 1.20)

library(draw) #para hacer rectangulo de fit

adj_y <- 0.6
adj_x <- 0.2
drawBox(x = 3.5, y = 1.2-adj_y, width = 7.5, height = 2,fillColor = "grey",opacity = 0)# marca espacio
drawBox(x = 3.5, y = 1.4-adj_y, width = 5.5, height = 1,fillColor = "grey",opacity = 0.5) # crea rectangulo

# paste0("Estimator DWLS", ", N=",nobs(msf_3.fit))
# paste0("Model fit: ", "χ²","(",msf_path[2,],")=",msf_path[1,],"***","; CFI=",msf_path[3,],"; RMSEA=",msf_path[4,])
# paste0("***p<0.001")

#pegar texto fit en rectangulo

size <- 10

drawText(x = 1.4+adj_x, y = 1.75-adj_y, text = paste0("Estimator: DWLS", ", N=",nobs(msf_3.fit)),family = "serif",size = size)
drawText(x = 2.3+adj_x, y = 1.59-adj_y, text = paste0("Model fit: ", "χ²","(",msf_path[2,],")=",msf_path[1,],"***",
                                                      "; CFI=",msf_path[3,],"; RMSEA=",msf_path[4,]),
         family = "serif",size = size)
drawText(x = 1.0+adj_x, y = 1.43-adj_y, text = paste0("***p<0.001"),family = "serif",size = size)

#guardar en png
drawExport("Output/images/atrib_cfa_path.png",units = "cm",width = 30,height = 20,ppi = 300)
