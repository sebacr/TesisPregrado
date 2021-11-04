pacman::p_load(dplyr, car, sjlabelled,labelled, stargazer,kableExtra,corrplot,
               tidyverse, ggplot2, ggpubr,haven, devtools,summarytools,nortest,tseries,psych,
               sjPlot,lavaan,semPlot,pander,standarize,reshape2,cowplot)
#install.packages("webshot",dependencies = TRUE)
library(webshot)
webshot::install_phantomjs(force = TRUE)
load("Input/Data_proc/data.RData") # Data percepcion 2019

data %>% 
  dplyr::select(atrib_pob_1,atrib_pob_2,atrib_pob_3,atrib_pob_4,atrib_pob_5,
                atrib_riq_1,atrib_riq_2,atrib_riq_3,atrib_riq_4,atrib_riq_5) -> atribcor

names(atribcor)
atribcor %>% 
  psych::describe() %>% 
  as.data.frame() %>% 
  dplyr::select("Mean"=mean,"SD"=sd,"Min"=min,"Max"=max) %>% 
  round(2) ->desc.issp
desc.issp



# Graficamos distribución de respuestas 

# Gráficos simples de barras

data %>%
  pivot_longer(atrib_pob_1:atrib_riq_5, names_to = "question", values_to = "response") %>%
  ggplot(aes(x = response)) +
  geom_bar() +
  facet_wrap(vars(question), ncol = 5) +
  labs(x = "Response (on a 1 to 5 scale)", y = "Number of respondents")





# Gráfico rectangular likert


dat_atrib <- atribcor %>% select("a.Falta de habilidad"=atrib_pob_1,"a.Mala suerte"=atrib_pob_2, "a.Falta esfuerzo"=atrib_pob_3, "a.Sistema económico"=atrib_pob_4, "a.Sistema educativo"=atrib_pob_5,"b.Talento"=atrib_riq_1,"b.Suerte"=atrib_riq_2, "b.Trabajo duro"=atrib_riq_3,"b.Sistema económico"=atrib_riq_4,"b.Sistema educativo"=atrib_riq_5)

dat_atrib_2 <- dat_atrib %>% sjmisc::rec(rec="rev") %>% 
  select(
    "a.Falta de habilidad" = "a.Falta de habilidad_r",
    "a.Mala suerte" = "a.Mala suerte_r",
    "a.Falta esfuerzo" = "a.Falta esfuerzo_r",
    "a.Sistema económico" = "a.Sistema económico_r",
    "a.Sistema educativo" = "a.Sistema educativo_r",
    "b.Talento" = b.Talento_r,
    "b.Suerte" = b.Suerte_r,
    "b.Trabajo duro" = "b.Trabajo duro_r",
    "b.Sistema económico" = "b.Sistema económico_r",
    "b.Sistema educativo" = "b.Sistema educativo_r")

# http://www.sthda.com/english/wiki/colors-in-r  = PALETAS de COLORES 
plotlikert<-plot_likert(dat_atrib_2, 
                        c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
                        groups.titles = c("Atribuciones pobreza", "Atribuciones riqueza"),
                        geom.colors = "BuPu",
                        geom.size = 0.8,
                        axis.labels = c("Por habilidad/talento","Por suerte","Por esfuerzo",
                                        "Por sistema económico","Por sistema educativo"),
                        catcount = 4,
                        cat.neutral = 3,
                        grid.range  =  c (1.2 , 1.4),
                        values  =  "sum.outside", 
                        reverse.colors = T,
                        reverse.scale = F)

plotlikert

ggsave(plotlikert,filename = "Output/images/plotlikert.png",device = "png",width = 30,height = 15,dpi = "retina",units = "cm")




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


















# EFA

# Primero chequear normalidad en variables dependientes

# Create correlation matrix
df_fa1 <- data %>% dplyr::select(atrib_pob_1,atrib_pob_2,atrib_pob_3,atrib_pob_4,atrib_pob_5,
                            atrib_riq_1,atrib_riq_2,atrib_riq_3,atrib_riq_4,atrib_riq_5) # Con variables fatalistas

df_fa2 <- data %>% dplyr::select(atrib_pob_1,atrib_pob_3,atrib_pob_4,atrib_pob_5,
                                 atrib_riq_1,atrib_riq_3,atrib_riq_4,atrib_riq_5) # Sin variables fatalistas


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
det(cor_df_fa2) # Resultado: 0.1641296 This value is greater than the necessary value of 0.00001 (see section 17.5). As such, our determinant does not seem problematic.

## KMO
KMO(df_fa1) # Media KMO 0.67 por lo que se puede realizar FA
KMO(df_fa2) # Media KMO 0.68 por lo que se puede realizar FA

# Barletts test
print(cortest.bartlett(df_fa1,nrow(data)))
print(cortest.bartlett(df_fa2,nrow(data)))
# Gráfico de sedimentacion

scree(cor_df_fa1)
scree(cor_df_fa2)

psych::fa.parallel(cor_df_fa1)
psych::fa.parallel(cor_df_fa2)

# Eigen value check
ev<-eigen(cor(cor_df_fa1))
ev

part.fa<-ev$values/sum(ev$values)*100
part.fa

#Plot a Scree plot using base plot:
Factor = c(1,2,3,4,5,6,7,8,9,10)
Eigen_Values <-ev$values
Scree <- data.frame(Factor, Eigen_Values)
plot(Scree, main = "Scree Plot", col= "Blue",ylim=c(0,4))
lines(Scree,col='Red')
abline(h = 1, col="Green")

#Plotting Scree plot using ggplot
library(ggplot2)
ggplot(data = Scree,mapping = aes(x=Factor,y=Eigen_Values))+
  geom_point()+
  geom_line()+
  scale_y_continuous(name="Eigen Values",limits = c(0,4))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank())+
  theme(panel.grid.major.y = element_line(colour = "skyblue"))+
  ggtitle("Scree Plot")







# EFA by PA con atribuciones fatalistas

#Modelo 1 factor
fa_pa.1<- fa(r=df_fa1, nfactors = 1, rotate = "oblimin", fm="pa")
fa_ml.1<- fa(r=df_fa1, nfactors = 1, rotate = "oblimin", fm="ml")
colnames(fa_pa.1$loadings) <- c("AT") #Cambio etiqueta factores
#rownames(fa_pa.1$loadings) <- c("Carab. Represión", "Carab. Desalojo", "Est. Piedras.", "Dano Inmobiliario.", "Dano Transporte.", "Dano Locales") # Cambio etiqueta indicadores
fa.sort(fa_ml.1)
fa.sort(fa_pa.1) # Resultados
fa.diagram(fa_pa.1) # Diagrama



# Modelo 2 factores 
fa_pa.2_fatal<- fa(r=df_fa1, nfactors = 2, rotate = "oblimin", fm="pa")
fa_ml.2_fatal<- fa(r=df_fa1, nfactors = 2, rotate = "oblimin", fm="ml")
colnames(fa_pa.2_fatal$loadings) <- c("AT. Int", "AT. Ext")
colnames(fa_ml.2_fatal$loadings) <- c("AT. Int", "AT. Ext")
#rownames(fa_pa.2_fatal$loadings) <- c("Carab. Represión", "Carab. Desalojo", "Est. Piedras.", "Dano Inmobiliario.", "Dano Transporte.", "Dano Locales")
fa.sort(fa_pa.2_fatal)
fa.sort(fa_ml.2_fatal)
fa.diagram(fa_pa.2_fatal)




#Modelo 3 factores
fa_pa.3<- fa(r=df_fa1, nfactors = 3, rotate = "oblimin", fm="pa")
colnames(fa_pa.3$loadings) <- c("AT. Int", "AT. Ext", "AT. Fatalis")
#rownames(fa_pa.3$loadings) <- c("Carab. Represión", "Carab. Desalojo", "Est. Piedras.", "Dano Inmobiliario.", "Dano Transporte.", "Dano Locales")
fa.sort(fa_pa.3)
fa.diagram(fa_pa.3)



#Modelo 4 factores
fa_pa.4<- fa(r=df_fa1, nfactors = 4, rotate = "oblimin", fm="pa") # maximum iteration exceeded









# EFA by PA sin atribuciones fatalistas


# Modelo 2 factores 
fa_pa.2<- fa(r=df_fa2, nfactors = 2, rotate = "oblimin", fm="pa")
colnames(fa_pa.2$loadings) <- c("AT. Int", "AT. Ext")
#rownames(fa_pa.2$loadings) <- c("Carab. Represión", "Carab. Desalojo", "Est. Piedras.", "Dano Inmobiliario.", "Dano Transporte.", "Dano Locales")
fa.sort(fa_pa.2)
fa.diagram(fa_pa.2)



# Modelo 3 factores 
fa_pa.3<- fa(r=df_fa2, nfactors = 3, rotate = "oblimin", fm="pa")
fa_ml.3<- fa(r=df_fa2, nfactors = 3, rotate = "oblimin", fm="ml")
colnames(fa_pa.3$loadings) <- c("AT. riq int", "AT. Ext","At. pob int")
colnames(fa_ml.3$loadings) <- c("AT. riq int", "AT. Ext","At. pob int")
#rownames(fa_pa.3$loadings) <- c("Carab. Represión", "Carab. Desalojo", "Est. Piedras.", "Dano Inmobiliario.", "Dano Transporte.", "Dano Locales")
fa.sort(fa_pa.3)
fa.sort(fa_ml.3)
fa.diagram(fa_pa.3)
fa.diagram(fa_ml.3)

# Modelo 4 factores 
fa_pa.4<- fa(r=df_fa2, nfactors = 4, rotate = "oblimin", fm="pa")
fa_ml.4<- fa(r=df_fa2, nfactors = 4, rotate = "oblimin", fm="ml")
colnames(fa_pa.4$loadings) <- c("AT. riq int", "AT. pob ext","AT. pob int","At. riq ext")
colnames(fa_ml.4$loadings) <- c("AT. riq int", "AT. pob int","AT. pob ext","At. riq ext")
#rownames(fa_pa.4$loadings) <- c("Carab. Represión", "Carab. Desalojo", "Est. Piedras.", "Dano Inmobiliario.", "Dano Transporte.", "Dano Locales")
fa.sort(fa_pa.4)
fa.sort(fa_ml.4)
fa.diagram(fa_pa.4)
fa.diagram(fa_ml.4)
















# CFA


# Crear base de datos limpiando outliers (19 casos por ahora)

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











# Modelos (se trabaja con bbdd sin outliers)




# Modelo con 2 factores incluyendo fatalismo


m2_fatal<-'
AI=~atrib_pob_1+atrib_pob_2+atrib_pob_3+atrib_riq_1+atrib_riq_2+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
'

m2_fatal.fit<-cfa(m2_fatal,data=df_noout,estimator="WLSMV",ordered=T)
summary(m2_fatal.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(m2_fatal.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

resid(m2_fatal.fit, type = "cor")


# Modificación de indices
modindices(m2_fatal.fit,sort. = T, minimum.value = 10.82)


## Crear modelos revisados 

m2_fatal2<-'
AI=~atrib_pob_1+atrib_pob_2+atrib_pob_3+atrib_riq_1+atrib_riq_2+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
atrib_pob_2 ~~ atrib_riq_2
'

m2_fatal2.fit<-cfa(m2_fatal2,data=data,estimator="WLSMV",ordered=T)
summary(m2_fatal2.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)


resid(m2_fatal2.fit, type = "cor")

# Modificación de indices

modindices(m2_fatal2.fit,sort. = T, minimum.value = 10.82)




m2_fatal3<-'
AI=~atrib_pob_1+atrib_pob_2+atrib_pob_3+atrib_riq_1+atrib_riq_2+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4
atrib_pob_2 ~~ atrib_riq_2
'

m2_fatal3.fit<-cfa(m2_fatal3,data=data,estimator="WLSMV",ordered=T)
summary(m2_fatal3.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)


resid(m2_fatal3.fit, type = "cor")

# Modificación de indices

modindices(m2_fatal3.fit,sort. = T, minimum.value = 10.82)

## Modelo con buenos ajustes quitando at riqueza "sistema educativo" y 
## considerando covariación entre atrib de suerte y entre atrib riq internas

m2_fatal4<-'
AI=~atrib_pob_1+atrib_pob_2+atrib_pob_3+atrib_riq_1+atrib_riq_2+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4
atrib_pob_2 ~~ atrib_riq_2
atrib_riq_1 ~~ atrib_riq_3
'

m2_fatal4.fit<-cfa(m2_fatal4,data=data,estimator="WLSMV",ordered=T)
summary(m2_fatal4.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

semPaths(m2_fatal4.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))















# Modelo de dos factores sin indicadores de fatalismo

model<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
'

fit<-cfa(model,data=df_noout,estimator="WLSMV",ordered=T)
summary(fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

resid(fit, type = "cor")


# Modificación de indices
modindices(fit,sort. = T, minimum.value = 10.82)



## Modelos revisados

revisado.model1<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4
'

revisado1.fit<-cfa(revisado.model1,data=data,estimator="WLSMV",ordered=T)
summary(revisado1.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

resid(revisado1.fit, type = "cor")

# Modificación de indices

modindices(revisado1.fit,sort. = T, minimum.value = 10.82)



# Modelo con ajustes suficientes. Alta covarianza entre atribuciones de pobreza internas, 
# lo cual puede indicar la ineficiencia de agregar algún indicador

revisado.model2<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4
atrib_pob_1 ~~ atrib_pob_3
'

revisado2.fit<-cfa(revisado.model2,data=data,ordered=T)
summary(revisado2.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(revisado2.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

resid(revisado2.fit, type = "cor")


# Modificación de indices

modindices(revisado2.fit,sort. = T, minimum.value = 10.82)







# Modelo con 3 factores incluyendo fatalismo


model_fatal<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4+atrib_riq_5
AF=~atrib_pob_2+atrib_riq_2
'

model_fatal.fit<-cfa(model_fatal,data=df_noout,estimator="WLSMV",ordered=T)
summary(model_fatal.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(model_fatal.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

resid(model_fatal.fit, type = "cor")


# Modificación de indices
modindices(model_fatal.fit,sort. = T, minimum.value = 10.82)






model_fatal2<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4
AF=~atrib_pob_2+atrib_riq_2
'

model_fatal2.fit<-cfa(model_fatal2,data=df_noout,estimator="WLSMV",ordered=T)
summary(model_fatal2.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

resid(model_fatal2.fit, type = "cor")


# Modificación de indices
modindices(model_fatal2.fit,sort. = T, minimum.value = 10.82)





# Modelo con buen ajuste.

model_fatal3<-'
AI=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
AE=~atrib_pob_4+atrib_pob_5+atrib_riq_4
AF=~atrib_pob_2+atrib_riq_2

# Correlaciones
atrib_riq_1 ~~ atrib_riq_3
'

model_fatal3.fit<-cfa(model_fatal3,data=data,estimator="WLSMV",ordered=T)
summary(model_fatal3.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(model_fatal3.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))


# Modificación de indices

modindices(model_fatal3.fit,sort. = T, minimum.value = 10.82)



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
























# Modelo con factores de segundo orden


model_SO1<-'
AI_pob=~atrib_pob_1+atrib_pob_3
AE_pob=~atrib_pob_4+atrib_pob_5
AI_riq=~atrib_riq_1+atrib_riq_3
AE_riq=~atrib_riq_4+atrib_riq_5
'

model_SO1.fit<-cfa(model_SO1,data=df_noout,ordered=T)
#standardizedSolution(m2_fatal.fit)
summary(model_SO1.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(model_SO1.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

resid(model_SO1.fit, type = "cor")


# Modificación de indices
modindices(model_SO1.fit,sort. = T, minimum.value = 10.82)






## Crear modelos revisados 
model_SO2<-'
AI_pob=~atrib_pob_1+atrib_pob_3
AE_pob=~atrib_pob_4+atrib_pob_5
AI_riq=~atrib_riq_1+atrib_riq_3+atrib_riq_4
AE_riq=~atrib_riq_4+atrib_riq_5
'

model_SO2.fit<-cfa(model_SO2,data=df_noout,ordered=T)
#standardizedSolution(m2_fatal.fit)
summary(model_SO2.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(model_SO2.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))
resid(model_SO2.fit, type = "cor")


# Modificación de indices
modindices(model_SO2.fit,sort. = T, minimum.value = 10.82)


#   #   #  Al alcanzar ajustes decentes, se prueba CFA de segundo orden


model_SO3<-'
AI_pob=~atrib_pob_1+atrib_pob_2+atrib_pob_3
AE_pob=~atrib_pob_4+atrib_pob_5
AI_riq=~atrib_riq_1+atrib_riq_2+atrib_riq_3+atrib_riq_4
AE_riq=~atrib_riq_4+atrib_riq_5

# Cov

atrib_pob_2 ~~ atrib_riq_2

# Segundo orden

AI=~AI_pob+AI_riq
AE=~AE_pob+AE_riq
'
model_SO3.fit<-cfa(model_SO3,data=df_noout,ordered=T) # WARNING: the optimizer warns that a solution
                                                      # has NOT been found! 
                                                      ### Se prueba cambiando el metodo de optimización 

model_SO3.fit<-cfa(model_SO3,data=df_noout,ordered=T,optim.method=list("BFGS")) # Al parecer no se tiene suficiente
                                                                                # información para ajustar los modelos





summary(model_SO3.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
# semPaths(model_SO3.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))
resid(model_SO3.fit, type = "cor")


# Modificación de indices
modindices(model_SO3.fit,sort. = T, minimum.value = 10.82)
