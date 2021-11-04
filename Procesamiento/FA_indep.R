pacman::p_load(dplyr, car, sjlabelled,labelled, stargazer,kableExtra,corrplot,
               tidyverse, ggplot2, ggpubr,haven, devtools,summarytools,nortest,tseries,psych,
               sjPlot,lavaan,semPlot,pander,standarize,reshape2,cowplot,GPArotation)
#install.packages("webshot",dependencies = TRUE)
library(webshot)
webshot::install_phantomjs(force = TRUE)
load("Input/Data_proc/data.RData") # Data percepcion 2019

data %>% 
  dplyr::select(merit_perc_effort,merit_perc_talent,merit_perc_wpart,merit_perc_netw,
                merit_pref_effort,merit_pref_talent,merit_pref_wpart,merit_pref_netw) -> meritcor


names(meritcor)
meritcor %>% 
  psych::describe() %>% 
  as.data.frame() %>% 
  dplyr::select("Mean"=mean,"SD"=sd,"Min"=min,"Max"=max) %>% 
  round(2) ->desc.issp
desc.issp



# Observar distribución de respuestas en variables dependientes

meritcor %>%
  pivot_longer(merit_perc_effort:merit_pref_netw, names_to = "question", values_to = "response") %>%
  ggplot(aes(x = response)) +
  geom_bar() +
  facet_wrap(vars(question), ncol = 3) +
  labs(x = "Response (on a 1 to 5 scale)", y = "Number of respondents")



# Grafico rectangulos likert


dat_merit <- meritcor %>% select("a.Esfuerzo"=merit_perc_effort,"a.Talento"=merit_perc_talent, 
                                 "a.Padres ricos"=merit_perc_wpart, "a.Contactos"=merit_perc_netw, 
                                 "b.Esfuerzo"=merit_pref_effort,"b.Talento"=merit_pref_talent,
                                 "b.Padres ricos"=merit_pref_wpart, "b.Contactos"=merit_pref_netw)

dat_merit_2<- dat_merit %>% sjmisc::rec(rec="rev") %>% 
  select(
    "a.Esfuerzo" = a.Esfuerzo_r,
    "a.Talento" = a.Talento_r,
    "a.Padres ricos" = "a.Padres ricos_r",
    "a.Contactos" = a.Contactos_r,
    "b.Esfuerzo" = b.Esfuerzo_r,
    "b.Talento" = b.Talento_r,
    "b.Padres ricos" = "b.Padres ricos_r",
    "b.Contactos" = b.Contactos_r)


plotlikert_merit<-plot_likert(dat_merit_2, 
                        c(1, 1, 1, 1, 2, 2, 2, 2),
                        groups.titles = c("Percepciones", "Preferencias"),
                        geom.colors = "PuBu",
                        geom.size = 0.8,
                        axis.labels = c("Esfuerzo", "Talento", "Padres ricos", "Contactos"),
                        catcount = 4,
                        cat.neutral = 3,
                        grid.range  =  c (1.2 , 1.4),
                        values  =  "sum.outside", 
                        reverse.colors = T,
                        reverse.scale = F)

plotlikert_merit

ggsave(plotlikert_merit,filename = "Output/images/plotlikert_merit.png",device = "png",width = 30,height = 15,dpi = "retina",units = "cm")




# Correlación de pearson

pearson_merit=cor(meritcor, use = "complete.obs")
windowsFonts(A = windowsFont("Times New Roman"))
rownames(pearson_merit) <-c(
  "(1) Obtienen mayores recompensas: Esfuerzo",
  "(2) Obtienen mayores recompensas: Talento",
  "(3) Logran salir adelante: Padres ricos",
  "(4) Logran salir adelante: Buenos contactos",
  "(5) Deberían obtener mayores recompensas: Esfuerzo",
  "(6) Deberían obtener mayores recompensas: Talento",
  "(7) Esta bien que salgan adelante: Padres ricos",
  "(8) Esta bien que salgan adelante: Buenos contactos")
colnames(pearson_merit) <-c("(1)", "(2)","(3)","(4)","(5)", "(6)","(7)","(8)")
corrplot(pearson_merit,
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

polycor_merit <- psych::polychoric(meritcor)
windowsFonts(A = windowsFont("Times New Roman"))
rownames(polycor_merit$rho) <-c(
  "(1) Obtienen mayores recompensas: Esfuerzo",
  "(2) Obtienen mayores recompensas: Talento",
  "(3) Logran salir adelante: Padres ricos",
  "(4) Logran salir adelante: Buenos contactos",
  "(5) Deberían obtener mayores recompensas: Esfuerzo",
  "(6) Deberían obtener mayores recompensas: Talento",
  "(7) Esta bien que salgan adelante: Padres ricos",
  "(8) Esta bien que salgan adelante: Buenos contactos")
colnames(polycor_merit$rho) <-c("(1)", "(2)","(3)","(4)","(5)", "(6)","(7)","(8)")
corrplot(polycor_merit$rho,
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


meritcor_fa =cor(meritcor, use = "complete.obs")
meritcor_fa<-as.data.frame(meritcor_fa)




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
is.recursive(meritcor)
lillie.test(meritcor$merit_perc_effort)
lillie.test(meritcor$merit_perc_talent)
lillie.test(meritcor$merit_perc_wpart)
lillie.test(meritcor$merit_perc_netw)
lillie.test(meritcor$merit_pref_effort)
lillie.test(meritcor$merit_pref_talent)
lillie.test(meritcor$merit_pref_wpart)
lillie.test(meritcor$merit_pref_netw)
jarque.bera.test(na.omit(meritcor$merit_perc_effort))
jarque.bera.test(na.omit(meritcor$merit_perc_talent))
jarque.bera.test(na.omit(meritcor$merit_perc_wpart))
jarque.bera.test(na.omit(meritcor$merit_perc_netw))
jarque.bera.test(na.omit(meritcor$merit_pref_effort))
jarque.bera.test(na.omit(meritcor$merit_pref_talent))
jarque.bera.test(na.omit(meritcor$merit_pref_wpart))
jarque.bera.test(na.omit(meritcor$merit_pref_netw))
## Determinant
det(meritcor_fa) # Resultado: 0.3133191 This value is greater than the necessary value of 0.00001 (see section 17.5). As such, our determinant does not seem problematic.

## KMO
KMO(meritcor) # 0.6

# Barletts test
print(cortest.bartlett(meritcor_fa,nrow(data)))


# Gráfico de sedimentacion

scree(meritcor_fa)

psych::fa.parallel(meritcor_fa)

# Eigen value check
ev<-eigen(cor(meritcor))
ev

part.fa<-ev$values/sum(ev$values)*100
part.fa

#Plot a Scree plot using base plot:
Factor = c(1,2,3,4,5,6,7,8)
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








# Modelo 2 factores 
fa_merit_pa.2<- fa(r=meritcor_fa, nfactors = 2, rotate = "oblimin", fm="pa")
fa_merit_ml.2<- fa(r=meritcor_fa, nfactors = 2, rotate = "oblimin", fm="ml")
colnames(fa_merit_pa.2$loadings) <- c("Merit", "No merit")
colnames(fa_merit_pa.2$loadings) <- c("Merit", "No merit")
#rownames(fa_pa.2_fatal$loadings) <- c("Carab. Represión", "Carab. Desalojo", "Est. Piedras.", "Dano Inmobiliario.", "Dano Transporte.", "Dano Locales")
fa.sort(fa_merit_pa.2)
fa.sort(fa_merit_ml.2)
fa.diagram(fa_merit_pa.2)



# Modelo 3 factores 
fa_merit_pa.3<- fa(r=meritcor_fa, nfactors = 3, rotate = "oblimin", fm="pa")
colnames(fa_merit_pa.3$loadings) <- c("Merit", "No merit perc", "No merit pref")
#rownames(fa_pa.2_fatal$loadings) <- c("Carab. Represión", "Carab. Desalojo", "Est. Piedras.", "Dano Inmobiliario.", "Dano Transporte.", "Dano Locales")
fa.sort(fa_merit_pa.3)
fa.diagram(fa_merit_pa.3)



# Modelo 4 factores
fa_merit_pa.4<- fa(r=meritcor_fa, nfactors = 4, rotate = "oblimin", fm="pa")
fa_merit_ml.4<- fa(r=meritcor_fa, nfactors = 4, rotate = "oblimin", fm="ml")
colnames(fa_merit_ml.4$loadings) <- c("Merit perc", "No merit perc", "No merit pref", "Merit pref")
colnames(fa_merit_pa.4$loadings) <- c("Merit perc", "No merit perc", "No merit pref", "Merit pref")
#rownames(fa_pa.2_fatal$loadings) <- c("Carab. Represión", "Carab. Desalojo", "Est. Piedras.", "Dano Inmobiliario.", "Dano Transporte.", "Dano Locales")
fa.sort(fa_merit_ml.4)
fa.sort(fa_merit_pa.4)
fa.diagram(fa_merit_ml.4)
fa.diagram(fa_merit_pa.4)


















# CFA


# Modelo de dos factores 

m_merit.2<-'
merit=~merit_pref_effort+merit_perc_talent+merit_perc_effort+merit_pref_talent
no_merit=~merit_perc_wpart+merit_perc_netw+merit_pref_wpart+merit_pref_netw
'

m_merit.2.fit<-cfa(m_merit.2,data=data,ordered=T)
summary(m_merit.2.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(m_merit.2.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

resid(m_merit.2.fit, type = "cor")

# Modificación de indices
modindices(m_merit.2.fit,sort. = T, minimum.value = 10.82)


# Modelo de tres factores 

m_merit.3<-'
merit=~merit_pref_effort+merit_perc_talent+merit_perc_effort+merit_pref_talent
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_wpart+merit_pref_netw
'

m_merit.3.fit<-cfa(m_merit.3,data=data,ordered=T)
summary(m_merit.3.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(m_merit.3.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

resid(m_merit.3.fit, type = "cor")


# Modificación de indices
modindices(m_merit.3.fit,sort. = T, minimum.value = 10.82)




m_merit.3B<-'
merit=~merit_pref_effort+merit_perc_talent+merit_perc_effort+merit_pref_talent
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_wpart+merit_pref_netw

# Cov
merit_pref_effort ~~ merit_pref_talent
'

m_merit.3B.fit<-cfa(m_merit.3B,data=data,ordered=T)
summary(m_merit.3B.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(m_merit.3B.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

resid(m_merit.3B.fit, type = "cor")


# Modificación de indices
modindices(m_merit.3B.fit,sort. = T, minimum.value = 10.82)




m_merit.3C<-'
merit=~merit_pref_effort+merit_perc_talent+merit_perc_effort+merit_pref_talent
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_wpart+merit_pref_netw
'

m_merit.3B.fit<-cfa(m_merit.3B,data=data,ordered=T)
summary(m_merit.3B.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(m_merit.3B.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

resid(m_merit.3B.fit, type = "cor")


# Modificación de indices
modindices(m_merit.3B.fit,sort. = T, minimum.value = 10.82)



# Modelo de cuatro factores 

m_merit.4<-'
perc_merit=~merit_perc_effort+merit_perc_talent
pref_merit=~merit_pref_talent+merit_pref_effort
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart
'

m_merit.4.fit<-cfa(m_merit.4,data=data,estimator="MLR",std.lv=FALSE)
#Continuous/ estimator ML Robust
m_merit.4_ord.fit <- cfa(model = m_merit.4,data = data,ordered = c("merit_perc_effort","merit_perc_talent",
                                                          "merit_perc_wpart","merit_perc_netw",
                                                          "merit_pref_effort","merit_pref_talent",
                                                          "merit_pref_wpart","merit_pref_netw"),std.lv=FALSE)

summary(m_merit.4.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
summary(m_merit.4_ord.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)


semPaths(m_merit.4_ord.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

resid(m_merit.4.fit, type = "cor")


# Modificación de indices
modindices(m_merit.4_ord.fit,sort. = T, minimum.value = 10.82)






# Segundo orden 

## Para pref y percep

merit_model_SO <- '
perc_merit=~merit_perc_effort+merit_perc_talent
pref_merit=~merit_pref_talent+merit_pref_effort
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart

# Second order
percep=~ perc_merit + perc_nmerit
prefer=~ pref_merit + pref_nmerit'

merit_model_SO.fit <- cfa(model = merit_model_SO,data = data, estimator="MLR")

merit_model_SO_ord.fit <- cfa(model = merit_model_SO,data = data,ordered = c("merit_perc_effort","merit_perc_talent",
                                                                                 "merit_perc_wpart","merit_perc_netw",
                                                                                 "merit_pref_effort","merit_pref_talent",
                                                                                 "merit_pref_wpart","merit_pref_netw"))


summary(merit_model_SO.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
summary(merit_model_SO_ord.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)






## Para merit y no-merit


merit_model_SO_2 <-'
perc_merit=~merit_perc_effort+merit_perc_talent
pref_merit=~merit_pref_talent+merit_pref_effort
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart

# SO
merit=~ perc_merit + pref_merit
nmerit=~ perc_nmerit + pref_nmerit'

# fit4b_c <- cfa(model = model04b2or,data = dat04,estimator="MLR") # No converge

merit_model_SO_2.fit <- cfa(model = merit_model_SO_2,data = data,
               ordered = c("merit_perc_effort","merit_perc_talent",
                           "merit_perc_wpart","merit_perc_netw",
                           "merit_pref_effort","merit_pref_talent",
                           "merit_pref_wpart","merit_pref_netw"))

summary(merit_model_SO_2.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)


# Modificación de indices
modindices(merit_model_SO_2.fit,sort. = T, minimum.value = 10.82)



# Agregar covarianza entre percepcion merit y percepcion no merit


merit_model_SO_2B <-'
perc_merit=~merit_perc_effort+merit_perc_talent
pref_merit=~merit_pref_talent+merit_pref_effort
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart

# SO
merit=~ perc_merit + pref_merit
nmerit=~ perc_nmerit + pref_nmerit

# Cov
perc_merit ~~ perc_nmerit'

merit_model_SO_2B.fit <- cfa(model = merit_model_SO_2B,data = data,
                              ordered = c("merit_perc_effort","merit_perc_talent",
                                          "merit_perc_wpart","merit_perc_netw",
                                          "merit_pref_effort","merit_pref_talent",
                                          "merit_pref_wpart","merit_pref_netw"))
summary(merit_model_SO_2B.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)









# Comparamos medidas de ajuste para modelo de 3 factores y 4 factores



sum_fit<- bind_rows(fitmeasures(m_merit.3.fit)[c("chisq.scaled","df","cfi.scaled","rmsea","rmsea.scaled")],
                    fitmeasures(m_merit.4_ord.fit)[c("chisq.scaled","df","cfi.scaled","rmsea","rmsea.scaled")])
sum_fit$mod <- c("Model 1","Model 2")
sum_fit$est <- c("DWLS","DWLS")
sum_fit <- select(sum_fit,mod,est,everything())
colnames <- c("Model","Estimator","$\\chi^2$","df","CFI.sca","RMSEA","RMSEA.sca")


sumtable02<- kable(sum_fit,digits = 3,format = "html",row.names = F,booktabs=T, caption = "Summary fit indices",col.names = colnames,escape = FALSE) %>%
  kable_styling(full_width = F)  %>%
  collapse_rows(columns = 1,valign = "middle")  %>%
  footnote(number = c("Model 1: Tres Factores",
                      "Model 2: Cuatro Factores"));sumtable02




m_merit.4_ord.fit$scores

# Agregamos variables indep a bbdd

regdata <- cbind(data[1], fa1$scores)
#Labeling the data

names(regdata) <- c("Satisfaction", "Purchase", "Marketing",
                    "Post_purchase", "Prod_positioning")
head(regdata)