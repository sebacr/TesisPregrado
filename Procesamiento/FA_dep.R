pacman::p_load(dplyr, car, sjlabelled,labelled, stargazer,kableExtra,corrplot,
               tidyverse, ggplot2, ggpubr,haven, devtools,summarytools,nortest,tseries,psych,
               sjPlot,lavaan,semPlot,pander,standarize)
#install.packages("webshot",dependencies = TRUE)
library(webshot)
webshot::install_phantomjs(force = TRUE)
load("Input/Data_proc/data.RData") # Data percepcion 2019

data %>% 
  dplyr::select(atrib_pob_1,atrib_pob_2,atrib_pob_3,atrib_pob_4,atrib_pob_5,
                atrib_riq_1,atrib_riq_2,atrib_riq_3,atrib_riq_2,atrib_riq_4,atrib_riq_5) %>% 
  cor(use = "complete.obs") -> atribcor

windowsFonts(A = windowsFont("Times New Roman"))
rownames(atribcor) <-c(
  "(1) Atribuciones pobreza Falta habilidad",
  "(2) Atribuciones pobreza Mala suerte",
  "(3) Atribuciones pobreza Falta de esfuerzo",
  "(4) Atribuciones pobreza Sistema económico",
  "(5) Atribuciones pobreza Sistema educativo",
  "(6) Atribuciones riqueza Talento",
  "(7) Atribuciones riqueza Suerte",
  "(8) Atribuciones riqueza Trabajo duro",
  "(9) Atribuciones riqueza Sistema económico",
  "(10) Atribuciones riqueza Sistema educativo")
colnames(atribcor) <-c("(1)", "(2)","(3)","(4)","(5)", "(6)","(7)","(8)","(9)","(10)")
corrplot(atribcor,
         method = "color",
         type = "upper",
         tl.col = "black",
         addCoef.col = "black",
         diag = TRUE,
         family = "A",
         number.font = 6,
         tl.cex =0.75,
         number.cex = 1)

# Observar distribución de respuestas en variables dependientes

data %>%
  pivot_longer(atrib_pob_1:atrib_riq_5, names_to = "question", values_to = "response") %>%
  ggplot(aes(x = response)) +
  geom_bar() +
  facet_wrap(vars(question), ncol = 3) +
  labs(x = "Response (on a 1 to 5 scale)", y = "Number of respondents")








# EFA

# Primero chequear normalidad en variables dependientes

# Create correlation matrix
df_fa1 <- data %>% dplyr::select(atrib_pob_1,atrib_pob_2,atrib_pob_3,atrib_pob_4,atrib_pob_5,
                            atrib_riq_1,atrib_riq_2,atrib_riq_3,atrib_riq_4,atrib_riq_5) # Var deps

df_fa2 <- data %>% dplyr::select(atrib_pob_1,atrib_pob_3,atrib_pob_4,atrib_pob_5,
                                 atrib_riq_1,atrib_riq_3,atrib_riq_4,atrib_riq_5) # Var deps


cor_df_fa1 =cor(df_fa1, use = "complete.obs")
cor_df_fa2 =cor(df_fa2, use = "complete.obs")
# Cronbach Aplha-s
#psych::alpha(cor_control, keys=NULL,cumulative=FALSE, title=NULL,na.rm = TRUE,
#             check.keys=TRUE,n.iter=1,delete=TRUE)
#psych::alpha(cor_cambio, keys=NULL,cumulative=FALSE, title=NULL,na.rm = TRUE,
#             check.keys=TRUE,n.iter=1,delete=TRUE)
#psych::alpha(cor_castigo, keys=NULL,cumulative=FALSE, title=NULL,na.rm = TRUE,
#             check.keys=TRUE,n.iter=1,delete=TRUE)
#psych::alpha(cor_df_fa1, keys=NULL,cumulative=FALSE, title=NULL,na.rm = TRUE,
#             check.keys=TRUE,n.iter=1,delete=TRUE)
## Normality test
lillie.test(df_fa1$atrib_pob_1)
lillie.test(df_fa1$atrib_pob_2)
lillie.test(df_fa1$atrib_pob_3)
lillie.test(df_fa1$atrib_pob_4)
lillie.test(df_fa1$atrib_pob_5)
lillie.test(df_fa1$atrib_riq_1)
lillie.test(df_fa1$atrib_riq_2)
lillie.test(df_fa1$atrib_riq_3)
lillie.test(df_fa1$atrib_riq_4)
lillie.test(df_fa1$atrib_riq_5)
jarque.bera.test(na.omit(df_fa1$atrib_pob_1))
jarque.bera.test(na.omit(df_fa1$atrib_pob_2))
jarque.bera.test(na.omit(df_fa1$atrib_pob_3))
jarque.bera.test(na.omit(df_fa1$atrib_pob_4))
jarque.bera.test(na.omit(df_fa1$atrib_pob_5))
jarque.bera.test(na.omit(df_fa1$atrib_riq_1))
jarque.bera.test(na.omit(df_fa1$atrib_riq_2))
jarque.bera.test(na.omit(df_fa1$atrib_riq_3))
jarque.bera.test(na.omit(df_fa1$atrib_riq_4))
jarque.bera.test(na.omit(df_fa1$atrib_riq_5))
## Determinant
det(cor_df_fa1) # Resultado: 0.1188577 This value is greater than the necessary value of 0.00001 (see section 17.5). As such, our determinant does not seem problematic.
det(cor_df_fa1) # Resultado: 0.1641296 This value is greater than the necessary value of 0.00001 (see section 17.5). As such, our determinant does not seem problematic.

## KMO
KMO(df_fa1)
KMO(df_fa2)

# Gráfico de sedimentacion

scree(cor_df_fa1)
scree(cor_df_fa2)

# EFA by PA

#Modelo 1 factor
fa_pa.1<- fa(r=df_fa1, nfactors = 1, rotate = "oblimin", fm="pa")
colnames(fa_pa.1$loadings) <- c("AT") #Cambio etiqueta factores
#rownames(fa_pa.1$loadings) <- c("Carab. Represión", "Carab. Desalojo", "Est. Piedras.", "Dano Inmobiliario.", "Dano Transporte.", "Dano Locales") # Cambio etiqueta indicadores
fa.sort(fa_pa.1) # Resultados
fa.diagram(fa_pa.1) # Diagrama





# Modelo 2 factores con indicadores fatalistas
fa_pa.2_fatal<- fa(r=df_fa1, nfactors = 2, rotate = "oblimin", fm="pa")
colnames(fa_pa.2_fatal$loadings) <- c("AT. Int", "AT. Ext")
#rownames(fa_pa.2_fatal$loadings) <- c("Carab. Represión", "Carab. Desalojo", "Est. Piedras.", "Dano Inmobiliario.", "Dano Transporte.", "Dano Locales")
fa.sort(fa_pa.2_fatal)
fa.diagram(fa_pa.2_fatal)

# Modelo 2 factores sin fatalismo
fa_pa.2<- fa(r=df_fa2, nfactors = 2, rotate = "oblimin", fm="pa")
colnames(fa_pa.2$loadings) <- c("AT. Int", "AT. Ext")
#rownames(fa_pa.2$loadings) <- c("Carab. Represión", "Carab. Desalojo", "Est. Piedras.", "Dano Inmobiliario.", "Dano Transporte.", "Dano Locales")
fa.sort(fa_pa.2)
fa.diagram(fa_pa.2)




#Modelo 3 factores
fa_pa.3<- fa(r=df_fa1, nfactors = 3, rotate = "oblimin", fm="pa")
colnames(fa_pa.3$loadings) <- c("AT. Int", "AT. Ext", "AT. Fatalis")
#rownames(fa_pa.3$loadings) <- c("Carab. Represión", "Carab. Desalojo", "Est. Piedras.", "Dano Inmobiliario.", "Dano Transporte.", "Dano Locales")
fa.sort(fa_pa.3)
fa.diagram(fa_pa.3)





#Modelo 4 factores
fa_pa.4<- fa(r=df_fa1, nfactors = 4, rotate = "oblimin", fm="pa") # maximum iteration exceeded












# CFA


# trabajar outliers

mahal = mahalanobis(data, colMeans(data, na.rm=T), 
                    cov(data, use="pairwise.complete"))

summary(mahal)

cutoff = qchisq(1-.001,ncol(data))

summary(mahal < cutoff)


df_noout = subset(data,mahal < cutoff)


correl = cor(df_noout, use="pairwise.complete.obs")

correl
symnum(correl)
corrplot.mixed(correl)

# examinar linealidad

random=rchisq(nrow(df_noout),7)

fake=lm(random~.,data=df_noout)

standarized=rstudent(fake)

qqnorm(standarized)
abline(0,1)

# examinar normalidad multivariada

hist(standarized)

# Examinar homogeneidad y homocedasticidad 

fitted=scale(fake$fitted.values)

plot(fitted,standarized)
abline(0,0)
abline(v=0)




# Modelos




# Modelo de dos factores sin indicadores de fatalismo

model<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
'

fit<-cfa(model,data=df_noout,estimator="WLSMV",ordered=T)
summary(fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

fitMeasures(fit)


# Modificación de indices
modindices(fit,sort. = T, minimum.value = 10.82)


# Ver correlacion entre errores

correl = residuals(fit,type='cor')
correl
#View (correl$cov)
#zcorrel=residuals(fit,tpye="standardized")






## Crear modelos revisados

revisado.model1<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
atrib_riq_1 ~~ atrib_riq_3
'

revisado1.fit<-cfa(revisado.model1,data=data,estimator="WLSMV",ordered=T)
summary(revisado1.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

# Modificación de indices

modindices(revisado1.fit,sort. = T, minimum.value = 10.82)



revisado.model2<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
atrib_riq_1 ~~ atrib_riq_3
atrib_riq_4 ~~ atrib_riq_5
'

revisado2.fit<-cfa(revisado.model2,data=data,estimator="WLSMV",ordered=T)
summary(revisado2.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

# Modificación de indices

modindices(revisado2.fit,sort. = T, minimum.value = 10.82)



revisado.model3<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
atrib_riq_1 ~~ atrib_riq_3
atrib_riq_4 ~~ atrib_riq_5
atrib_riq_1 ~~ atrib_riq_5
'

revisado3.fit<-cfa(revisado.model3,data=data,estimator="WLSMV",ordered=T)
summary(revisado3.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

# Modificación de indices

modindices(revisado3.fit,sort. = T, minimum.value = 10.82)



revisado.model4<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
atrib_riq_1 ~~ atrib_riq_3
atrib_riq_4 ~~ atrib_riq_5
atrib_riq_1 ~~ atrib_riq_5
atrib_riq_3 ~~ atrib_riq_5
'

revisado4.fit<-cfa(revisado.model4,data=data,estimator="WLSMV",ordered=T)
summary(revisado4.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

# Modificación de indices

modindices(revisado3.fit,sort. = T, minimum.value = 10.82)



  
revisado.model5<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
atrib_riq_1 ~~ atrib_riq_3
atrib_riq_4 ~~ atrib_riq_5
atrib_riq_1 ~~ atrib_riq_5
atrib_riq_3 ~~ atrib_riq_5
atrib_pob_5 ~~ atrib_riq_5
'

revisado5.fit<-cfa(revisado.model5,data=data,estimator="WLSMV",ordered=T)
summary(revisado5.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(revisado5.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))










# Modelo con 3 factores incluyendo fatalismo


model_fatal<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
AF=~atrib_pob_2+atrib_riq_2
'

model_fatal.fit<-cfa(model_fatal,data=df_noout,estimator="WLSMV",ordered=T)
summary(model_fatal.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(model_fatal.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

fitMeasures(model_fatal.fit)


# Modificación de indices
modindices(model_fatal.fit,sort. = T, minimum.value = 10.82)


# Ver correlacion entre errores

correl = residuals(model_fatal.fit,type='cor')
correl
#View (correl$cov)
#zcorrel=residuals(fit,tpye="standardized")




## Crear modelos revisados

modelfatal_rev_1<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
AF=~atrib_pob_2+atrib_riq_2

# Correlaciones
atrib_riq_1 ~~ atrib_riq_3
'

fatal.fit1<-cfa(modelfatal_rev_1,data=data,estimator="WLSMV",ordered=T)
summary(fatal.fit1,fit.measures=TRUE,standardized=TRUE,rsquare=T)

# Modificación de indices

modindices(fatal.fit1,sort. = T, minimum.value = 10.82)



modelfatal_rev_2<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
AF=~atrib_pob_2+atrib_riq_2

# Correlaciones
atrib_riq_1 ~~ atrib_riq_3
atrib_riq_4 ~~ atrib_riq_5
'

fatal.fit2<-cfa(modelfatal_rev_2,data=data,estimator="WLSMV",ordered=T)
summary(fatal.fit2,fit.measures=TRUE,standardized=TRUE,rsquare=T)

# Modificación de indices

modindices(fatal.fit2,sort. = T, minimum.value = 10.82)



modelfatal_rev_3<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
AF=~atrib_pob_2+atrib_riq_2

# Correlaciones
atrib_riq_1 ~~ atrib_riq_3
atrib_riq_4 ~~ atrib_riq_5
atrib_riq_1 ~~ atrib_riq_5
'

fatal.fit3<-cfa(modelfatal_rev_3,data=data,estimator="WLSMV",ordered=T)
summary(fatal.fit3,fit.measures=TRUE,standardized=TRUE,rsquare=T)

# Modificación de indices

modindices(fatal.fit3,sort. = T, minimum.value = 10.82)



modelfatal_rev_4<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
AF=~atrib_pob_2+atrib_riq_2

# Correlaciones
atrib_riq_1 ~~ atrib_riq_3
atrib_riq_4 ~~ atrib_riq_5
atrib_riq_1 ~~ atrib_riq_5
atrib_riq_3 ~~ atrib_riq_5
'

fatal.fit4<-cfa(modelfatal_rev_4,data=data,estimator="WLSMV",ordered=T)
summary(fatal.fit4,fit.measures=TRUE,standardized=TRUE,rsquare=T)

# Modificación de indices

modindices(fatal.fit4,sort. = T, minimum.value = 10.82)





modelfatal_rev_5<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
AF=~atrib_pob_2+atrib_riq_2

# Correlaciones
atrib_riq_1 ~~ atrib_riq_3
atrib_riq_4 ~~ atrib_riq_5
atrib_riq_1 ~~ atrib_riq_5
atrib_riq_3 ~~ atrib_riq_5
atrib_pob_5 ~~ atrib_riq_5
'

fatal.fit5<-cfa(modelfatal_rev_5,data=data,estimator="WLSMV",ordered=T)
summary(fatal.fit5,fit.measures=TRUE,standardized=TRUE,rsquare=T)

# Modificación de indices

modindices(fatal.fit5,sort. = T, minimum.value = 10.82)

semPaths(fatal.fit5,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))









# Modelo con 2 factores incluyendo fatalismo


m2_fatal<-'
AI=~atrib_pob_1+atrib_pob_2+atrib_pob_3+atrib_riq_1+atrib_riq_2+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
'

m2_fatal.fit<-cfa(m2_fatal,data=df_noout,estimator="WLSMV",ordered=T)
summary(m2_fatal.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(m2_fatal.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

fitMeasures(m2_fatal.fit)


# Modificación de indices
modindices(m2_fatal.fit,sort. = T, minimum.value = 10.82)


# Ver correlacion entre errores

correl = residuals(model_fatal.fit,type='cor')
correl
#View (correl$cov)
#zcorrel=residuals(fit,tpye="standardized")




## Crear modelos revisados 

m2_fatal_2<-'
AI=~atrib_pob_1+atrib_pob_2+atrib_pob_3+atrib_riq_1+atrib_riq_2+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5

# Correlaciones
atrib_pob_2 ~~ atrib_riq_2
'

m2_fatal_2.fit<-cfa(m2_fatal_2,data=data,estimator="WLSMV",ordered=T)
summary(m2_fatal_2.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

# Modificación de indices

modindices(m2_fatal_2.fit,sort. = T, minimum.value = 10.82)



m2_fatal_3<-'
AI=~atrib_pob_1+atrib_pob_2+atrib_pob_3+atrib_riq_1+atrib_riq_2+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5

# Correlaciones
atrib_pob_2 ~~ atrib_riq_2
atrib_riq_1 ~~ atrib_riq_3
'

m2_fatal_3.fit<-cfa(m2_fatal_3,data=data,estimator="WLSMV",ordered=T)
summary(m2_fatal_3.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

# Modificación de indices

modindices(m2_fatal_3.fit,sort. = T, minimum.value = 10.82)


m2_fatal_4<-'
AI=~atrib_pob_1+atrib_pob_2+atrib_pob_3+atrib_riq_1+atrib_riq_2+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5

# Correlaciones
atrib_pob_2 ~~ atrib_riq_2
atrib_riq_1 ~~ atrib_riq_3
atrib_riq_4 ~~ atrib_riq_5
'

m2_fatal_4.fit<-cfa(m2_fatal_4,data=data,estimator="WLSMV",ordered=T)
summary(m2_fatal_4.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

# Modificación de indices

modindices(m2_fatal_4.fit,sort. = T, minimum.value = 10.82)


m2_fatal_4<-'
AI=~atrib_pob_1+atrib_pob_2+atrib_pob_3+atrib_riq_1+atrib_riq_2+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5

# Correlaciones
atrib_pob_2 ~~ atrib_riq_2
atrib_riq_1 ~~ atrib_riq_3
atrib_riq_4 ~~ atrib_riq_5
atrib_riq_1 ~~ atrib_riq_5
'

m2_fatal_4.fit<-cfa(m2_fatal_4,data=data,estimator="WLSMV",ordered=T)
summary(m2_fatal_4.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

# Modificación de indices

modindices(m2_fatal_4.fit,sort. = T, minimum.value = 10.82)


m2_fatal_5<-'
AI=~atrib_pob_1+atrib_pob_2+atrib_pob_3+atrib_riq_1+atrib_riq_2+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5

# Correlaciones
atrib_pob_2 ~~ atrib_riq_2
atrib_riq_1 ~~ atrib_riq_3
atrib_riq_4 ~~ atrib_riq_5
atrib_riq_1 ~~ atrib_riq_5
atrib_riq_3 ~~ atrib_riq_5
'

m2_fatal_5.fit<-cfa(m2_fatal_5,data=data,estimator="WLSMV",ordered=T)
summary(m2_fatal_5.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

# Modificación de indices

modindices(m2_fatal_5.fit,sort. = T, minimum.value = 10.82)


m2_fatal_6<-'
AI=~atrib_pob_1+atrib_pob_2+atrib_pob_3+atrib_riq_1+atrib_riq_2+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5

# Correlaciones
atrib_pob_2 ~~ atrib_riq_2
atrib_riq_1 ~~ atrib_riq_3
atrib_riq_4 ~~ atrib_riq_5
atrib_riq_1 ~~ atrib_riq_5
atrib_riq_3 ~~ atrib_riq_5
atrib_pob_5 ~~ atrib_riq_5
'

m2_fatal_6.fit<-cfa(m2_fatal_6,data=data,estimator="WLSMV",ordered=T)
summary(m2_fatal_6.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

# Modificación de indices

modindices(m2_fatal_6.fit,sort. = T, minimum.value = 10.82)


m2_fatal_7<-'
AI=~atrib_pob_1+atrib_pob_2+atrib_pob_3+atrib_riq_1+atrib_riq_2+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5

# Correlaciones
atrib_pob_2 ~~ atrib_riq_2
atrib_riq_1 ~~ atrib_riq_3
atrib_riq_4 ~~ atrib_riq_5
atrib_riq_1 ~~ atrib_riq_5
atrib_riq_3 ~~ atrib_riq_5
atrib_pob_5 ~~ atrib_riq_5
atrib_pob_4 ~~ atrib_riq_5
'

m2_fatal_7.fit<-cfa(m2_fatal_7,data=data,estimator="WLSMV",ordered=T)
summary(m2_fatal_7.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

# Modificación de indices

modindices(m2_fatal_7.fit,sort. = T, minimum.value = 10.82)


semPaths(m2_fatal_7.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))
