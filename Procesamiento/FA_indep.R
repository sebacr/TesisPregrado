pacman::p_load(dplyr, car, sjlabelled,labelled, stargazer,kableExtra,corrplot,
               tidyverse, ggplot2, ggpubr,haven, devtools,summarytools,nortest,tseries,psych,
               sjPlot,lavaan,semPlot,pander,standarize,wesanderson,semTools,ggpubr)
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


# Grafico rectangulos likert


dat_merit <- meritcor %>% dplyr::select("a.Esfuerzo"=merit_perc_effort,"a.Talento"=merit_perc_talent,
                                 "a.Padres ricos"=merit_perc_wpart, "a.Contactos"=merit_perc_netw,
                                 "b.Esfuerzo"=merit_pref_effort,"b.Talento"=merit_pref_talent,
                                 "b.Padres ricos"=merit_pref_wpart, "b.Contactos"=merit_pref_netw)
dat_merit <- set_label(dat_merit, c("Esfuerzo","Talento","Padres ricos",
                                    "Buenos contactos","Esfuerzo","Talento","Padres ricos",
                                    "Buenos contactos"))

dat_merit_2<- dat_merit %>% sjmisc::rec(rec="rev") %>%
dplyr::select(
  "a.Esfuerzo" = a.Esfuerzo_r,
  "a.Talento" = a.Talento_r,
  "a.Padres ricos" = "a.Padres ricos_r",
  "a.Contactos" = a.Contactos_r,
  "b.Esfuerzo" = b.Esfuerzo_r,
  "b.Talento" = b.Talento_r,
  "b.Padres ricos" = "b.Padres ricos_r",
  "b.Contactos" = b.Contactos_r)


set_theme(
  base = theme_light(),
  theme.font = 'serif',
  axis.title.size = .9,
  axis.textsize = .9,
  legend.size = .7,
  legend.title.size = .8,
  geom.label.size = 3
)
merit_likert<-plot_likert(dat_merit_2,
                              c(1, 1, 1, 1, 2, 2, 2, 2),
                              groups.titles = c("Percepciones", "Preferencias"),
                          geom.colors   = c("#6baed6","#9ecae1","#FF9999", 
                                            "#FF5555"),
                              geom.size = 0.8,
                              catcount = 4,
                              cat.neutral = 3,
                              grid.range  =  c (1.2 , 1.4),
                              values  =  "sum.outside",
                              reverse.colors = T,
                              reverse.scale = F)

merit_likert
ggsave(merit_likert,filename = "Output/images/merit_likert.png",device = "png",width = 30,height = 15,dpi = "retina",units = "cm")


# Observar distribución de respuestas en variables dependientes

meritcor %>%
  pivot_longer(merit_perc_effort:merit_pref_netw, names_to = "question", values_to = "response") %>%
  ggplot(aes(x = response)) +
  geom_bar() +
  facet_wrap(vars(question), ncol = 3) +
  labs(x = "Respuestas (escala de 1 a 5)", y = "Número de respuestas")



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


merit_fa =cor(meritcor, use = "complete.obs")

merit_fa<-as.data.frame(merit_fa)


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
is.recursive(merit_fa)
lillie.test(merit_fa$merit_perc_effort)
lillie.test(merit_fa$merit_perc_talent)
lillie.test(merit_fa$merit_perc_wpart)
lillie.test(merit_fa$merit_perc_netw)
lillie.test(merit_fa$merit_pref_effort)
lillie.test(merit_fa$merit_pref_talent)
lillie.test(merit_fa$merit_pref_wpart)
lillie.test(merit_fa$merit_pref_netw)
jarque.bera.test(na.omit(merit_fa$merit_perc_effort))
jarque.bera.test(na.omit(merit_fa$merit_perc_talent))
jarque.bera.test(na.omit(merit_fa$merit_perc_wpart))
jarque.bera.test(na.omit(merit_fa$merit_perc_netw))
jarque.bera.test(na.omit(merit_fa$merit_pref_effort))
jarque.bera.test(na.omit(merit_fa$merit_pref_talent))
jarque.bera.test(na.omit(merit_fa$merit_pref_wpart))
jarque.bera.test(na.omit(merit_fa$merit_pref_netw))

## Determinant

merit_fa<-as.matrix(merit_fa)
det(merit_fa) # Resultado: 0.3133191 This value is greater than the necessary value of 0.00001 (see section 17.5). As such, our determinant does not seem problematic.

## KMO
KMO(merit_fa) # 0.6

# Gráfico de sedimentacion

scree(merit_fa)



# Modelo 2 factores 
fa_merit_pa.2<- fa(r=merit_fa, nfactors = 2, rotate = "oblimin", fm="pa")
#fa_merit_ml.2<- fa(r=merit_fa, nfactors = 2, rotate = "oblimin", fm="ml")
colnames(fa_merit_pa.2$loadings) <- c("Merit", "No merit")
#colnames(fa_merit_ml.2$loadings) <- c("Merit", "No merit")
fa.sort(fa_merit_pa.2)
#fa.sort(fa_merit_ml.2)
fa.diagram(fa_merit_pa.2)

merit_f2_table <- fa_table(fa_merit_pa.2, -10)
rownames(merit_f2_table)  <- c("Obtienen mayores recompensas: Esfuerzo","Obtienen mayores recompensas: Talento","Logran salir adelante: Padres ricos","Logran salir adelante: Buenos contactos","Deberían obtener mayores recompensas: Esfuerzo","Deberían obtener mayores recompensas: Talento","Esta bien que salgan adelante: Padres ricos","Esta bien que salgan adelante: Buenos contactos")
merit_f2_table <- dplyr::select(merit_f2_table, -item)

# Modelo 3 factores

fa_merit_pa.3<- fa(r=merit_fa, nfactors = 3, rotate = "oblimin", fm="pa")
colnames(fa_merit_pa.3$loadings) <- c("Percepciones meritocráticas + no meritocráticas", 
                                      "Preferencias meritocráticas", "Preferencias No meritocráticas")
fa.sort(fa_merit_pa.3)
fa.diagram(fa_merit_pa.3)

merit_f3_table <- fa_table(fa_merit_pa.3, -10)
rownames(merit_f3_table)  <- c("Obtienen mayores recompensas: Esfuerzo","Obtienen mayores recompensas: Talento","Logran salir adelante: Padres ricos","Logran salir adelante: Buenos contactos","Deberían obtener mayores recompensas: Esfuerzo","Deberían obtener mayores recompensas: Talento","Esta bien que salgan adelante: Padres ricos","Esta bien que salgan adelante: Buenos contactos")
merit_f3_table <- dplyr::select(merit_f3_table, -item)

# Modelo 4 factores 
fa_merit_pa.4<- fa(r=merit_fa, nfactors = 4, rotate = "oblimin", fm="pa", max.iter = 500)
colnames(fa_merit_pa.4$loadings) <- c("Percep Merit","Pref Merit", "Percep No merit", "Pref No merit")
fa.sort(fa_merit_pa.4)
fa.diagram(fa_merit_pa.4)











# CFA



# Modelo de dos factores 

m_merit.2<-'
merit=~merit_pref_effort+merit_perc_talent+merit_perc_effort+merit_pref_talent
nomerit=~merit_perc_wpart+merit_perc_netw+merit_pref_wpart+merit_pref_netw

# Cov
merit_perc_wpart ~~ merit_perc_netw
merit_pref_effort ~~ merit_pref_talent
merit_perc_effort ~~ merit_pref_talent
merit_perc_wpart ~~  merit_pref_wpart'

m_merit.2.fit<-cfa(m_merit.2,data=data,estimator="WLSMV",ordered=T)

summary(m_merit.2.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T) # mal ajuste a pesar de 
                                                                     # aplicar modif

semPaths(m_merit.2.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

#resid(m_merit.3.fit, type = "cor")


# Modificación de indices
modindices(m_merit.2.fit,sort. = T, minimum.value = 10.82)





# Modelo de tres factores 

m_merit.3<-'
merit=~merit_pref_effort+merit_perc_talent+merit_perc_effort+merit_pref_talent
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_wpart+merit_pref_netw

# Cov
merit_pref_effort ~~ merit_pref_talent
merit_perc_effort ~~ merit_pref_talent
'

m_merit.3.fit<-cfa(m_merit.3,data=data,estimator="WLSMV",ordered=T)
summary(m_merit.3.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T) # Mal ajuste a pesar de
                                                                     # aplicar modif 
semPaths(m_merit.3.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

#resid(m_merit.3.fit, type = "cor")


# Modificación de indices
modindices(m_merit.3.fit,sort. = T, minimum.value = 10.82)








# Modelo de cuatro factores 

m_merit.4<-'
perc_merit=~merit_perc_effort+merit_perc_talent
pref_merit=~merit_pref_talent+merit_pref_effort
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart
'

#m_merit.4.fit<-cfa(m_merit.4,data=data,missing = "ML")
#Continuous/ estimator ML Robust
m_merit.4_ord.fit <- cfa(model = m_merit.4,estimator="WLSMV",data = data,ordered = T)

#summary(m_merit.4.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
summary(m_merit.4_ord.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T) # MEJOR MODELO

semPaths(m_merit.4_ord.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

resid(m_merit.4_ord.fit, type = "cor")

# Modificación de indices
modindices(m_merit.4_ord.fit,sort. = T, minimum.value = 10.82)

nullRMSEA(m_merit.4_ord.fit, scaled = FALSE, silent = FALSE)

m4_rmsea_adj<-'
perc_merit=~merit_perc_effort+merit_perc_talent
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart
'
m4_rmsea_adj.fit <- cfa(model = m4_rmsea_adj,estimator="WLSMV",data = data,ordered = T)

save(m_merit.4_ord.fit,file = "Output/Tablas/m_merit.4_ord.fit.RData")
save(m4_rmsea_adj.fit,file = "Output/Tablas/m4_rmsea_adj.fit.RData")



# Segundo orden 

## Para pref y percep

merit_4_2doorder <- '
merit=~merit_perc_effort+merit_perc_talent+merit_pref_talent+merit_pref_effort
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart
prefer=~ perc_nmerit + pref_nmerit'

merit_4_2doorder.fit <- cfa(model = merit_4_2doorder,data = data, estimator="MLR")

merit_4_2doorder_ord.fit <- cfa(model = merit_4_2doorder,data = data,ordered = T)


summary(merit_4_2doorder.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
summary(merit_4_2doorder_ord.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T) # Mal ajuste






## Para merit y no-merit


merit_4_2doorder_b <-'
perc_merit=~merit_perc_effort+merit_perc_talent
pref_merit=~merit_pref_talent+merit_pref_effort
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart
merit=~ perc_merit + pref_merit
nmerit=~ perc_nmerit + pref_nmerit

# Cov
pref_merit ~~ pref_nmerit
'

#fit4b_c <- cfa(model = merit_4_2doorder_b,data = data,estimator="MLR") # No converge

merit_4_2doorder_b.fit <- cfa(model = merit_4_2doorder_b,data = data,
               ordered = T, control=list(iter.max=10000)) # No hay solucion

#summary(fit4b_c,fit.measures=TRUE,standardized=TRUE,rsquare=T)
summary(merit_4_2doorder_b.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semPaths(merit_4_2doorder_b.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))

# Modificación de indices
modindices(merit_4_2doorder_b.fit,sort. = T, minimum.value = 10.82)















m_merit.4.modif<-'
perc_merit=~merit_perc_effort+merit_perc_talent
pref_merit=~merit_pref_talent+merit_pref_effort
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart

# Cov
merit_perc_effort ~~ merit_pref_talent
'
m_4.fit <- cfa(model = m_merit.4.modif,data = data,ordered = T)
#summary(m_4.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

indep_labs <- c(
  "Obtienen mayores recompensas: Esfuerzo","Obtienen mayores recompensas: Talento",
  "Logran salir adelante: Padres ricos","Logran salir adelante: Buenos contactos",
  "Deberían obtener mayores recompensas: Esfuerzo","Deberían obtener mayores recompensas: Talento",
  "Esta bien que salgan adelante: Padres ricos","Esta bien que salgan adelante: Buenos contactos"
)
tb.load<- data.frame(round(cbind(lavaan::inspect(m_4.fit,
                                                 what="std")$lambda),
                           digits = 2))
tb.load[tb.load==c(0.00)] <- NA

for (i in names(tb.load)) {
  # tb.load[,i] <- sjlabelled::as_character(tb.load[,i])
  tb.load[,i] <- sprintf(tb.load[,i], fmt = '%#.2f')
}
tb.load[tb.load=="NA"] <- ""
#-------#
fm01<- data.frame(t(data.frame(lavaan::fitmeasures(m_4.fit, output ="matrix")[c("chisq","df","cfi","tli","rmsea"),]))); row.names(fm01) ="perc_merit"

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
fm.df$nobs <- c(lavaan::nobs(m_4.fit)) 
fm.df <- data.frame(t(fm.df)); colnames(fm.df) <- c("perc_merit")


#------ merge ------#
tb.fm<- dplyr::bind_rows(tb.load,fm.df)
tb.fm<- tb.fm %>% 
  dplyr::mutate(Variables=c(indep_labs,"$\\chi^2\\text{(df)}$","$\\text{CFI}$",
                            "$\\text{TLI}$","$\\text{RMSEA}$","$N$")) %>%
  dplyr::select(Variables,everything())
tb.merit.fit.4f <- tb.fm

save(tb.merit.fit.4f,file = "Output/Tablas/tb.merit.fit.4f.RData")






# Path graph

m_merit.4<-'
perc_merit=~merit_perc_effort+merit_perc_talent
pref_merit=~merit_pref_talent+merit_pref_effort
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart
'
m_merit.4_ord.fit <- cfa(model = m_merit.4,data = data,ordered = T)
#semPaths(m_merit.4_ord.fit,whatLabels = "std",layout = "tree",edge.label.cex = 0.80, mar=c(3,3,3,3))
summary(m_merit.4_ord.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

# Modificación de indices
modindices(m_merit.4_ord.fit,sort. = T, minimum.value = 10.82)

nodeNames <-c(
  "Obtienen mayores \n recompensas: Esfuerzo","Obtienen mayores \n recompensas: Talento",
  "Deberían obtener mayores \n recompensas: Esfuerzo","Deberían obtener mayores \n recompensas: Talento",
  "Logran salir adelante: \n Padres ricos","Logran salir adelante: \n Buenos contactos",
  "Esta bien que salgan adelante: \n Padres ricos","Esta bien que salgan adelante: \n  Buenos contactos",
              "Percepción \n meritocrática","Preferencia \n Meritocrática","Percepción \n no meritocrática",
  "Preferencia \n no meritocrática")

# graphics.off()
fm03<- data.frame(v1=fitmeasures(m_merit.4_ord.fit, output ="matrix")[c("chisq.scaled","df","cfi.scaled","rmsea.scaled"),])
fm03 <- round(fm03,3)
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
  m_merit.4_ord.fit ,
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
  sizeLat2 = 18, # alto de los circulos
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

ld<- standardizedsolution(m_merit.4_ord.fit) %>% dplyr::select(lhs,op,rhs,est.std) %>% filter(op=="=~") 
ld$est.std<- sprintf("%.2f", ld$est.std)

#pegar cargas factoriales al plot
text(x = -0.1,y =  1.00, ld$est.std[1],font = 5,cex = 1.20)
text(x = -0.1,y =  0.85, ld$est.std[2],font = 5,cex = 1.20)
text(x = -0.1,y =  0.43, ld$est.std[3],font = 5,cex = 1.20)
text(x = -0.1,y =  0.26, ld$est.std[4],font = 5,cex = 1.20)
text(x = -0.1,y = -0.15, ld$est.std[5],font = 5,cex = 1.20)
text(x = -0.1,y = -0.30, ld$est.std[6],font = 5,cex = 1.20)
text(x = -0.1,y = -0.73, ld$est.std[7],font = 5,cex = 1.20)
text(x = -0.1,y = -0.88, ld$est.std[8],font = 5,cex = 1.20)
text(x = 1.31,y = 0.52, "0.53",font = 5,cex = 1.20)
text(x = 1.57,y = 0, "0.4",font = 5,cex = 1.20)
text(x = 1.30,y = 0, "0.36",font = 5,cex = 1.20)

library(draw) #para hacer rectangulo de fit

adj_y <- 0.6
adj_x <- 0.2
drawBox(x = 3.5, y = 1.2-adj_y, width = 7.5, height = 2,fillColor = "grey",opacity = 0)# marca espacio
drawBox(x = 3.5, y = 1.4-adj_y, width = 5.5, height = 1,fillColor = "grey",opacity = 0.5) # crea rectangulo

# paste0("Estimator DWLS", ", N=",nobs(m_merit.4_ord.fit))
# paste0("Model fit: ", "χ²","(",fm03[2,],")=",fm03[1,],"***","; CFI=",fm03[3,],"; RMSEA=",fm03[4,])
# paste0("***p<0.001")

#pegar texto fit en rectangulo

size <- 10

drawText(x = 1.4+adj_x, y = 1.75-adj_y, text = paste0("Estimator: DWLS", ", N=",nobs(m_merit.4_ord.fit)),family = "serif",size = size)
drawText(x = 2.3+adj_x, y = 1.59-adj_y, text = paste0("Model fit: ", "χ²","(",fm03[2,],")=",fm03[1,],"***","; CFI=",fm03[3,],"; RMSEA=",fm03[4,]),family = "serif",size = size)
drawText(x = 1.0+adj_x, y = 1.43-adj_y, text = paste0("***p<0.001"),family = "serif",size = size)


#guardar en png
drawExport("Output/images/merit_cfa_path.png",units = "cm",width = 30,height = 20,ppi = 300)










# Gráfico para modelo sin preferencias meritocráticas



m4_rmsea_adj<-'
perc_merit=~merit_perc_effort+merit_perc_talent
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart
'
m4_rmsea_adj.fit <- cfa(model = m4_rmsea_adj,estimator="WLSMV",data = data,ordered = T)
summary(m4_rmsea_adj.fit,fit.measures=TRUE,standardized=TRUE,rsquare=T)

nodeNames2 <-c(
  "Obtienen mayores \n recompensas: Esfuerzo","Obtienen mayores \n recompensas: Talento",
  "Logran salir adelante: \n Padres ricos","Logran salir adelante: \n Buenos contactos",
  "Esta bien que salgan adelante: \n Padres ricos","Esta bien que salgan adelante: \n  Buenos contactos",
  "Percepción \n meritocrática","Percepción \n no meritocrática",
  "Preferencia \n no meritocrática")

# graphics.off()
fm333<- data.frame(v1=fitmeasures(m4_rmsea_adj.fit, output ="matrix")[c("chisq.scaled","df","cfi.scaled","rmsea.scaled"),])
fm333 <- round(fm333,3)
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
  m4_rmsea_adj.fit ,
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
  # nodeNames = nodeNames2,
  intercepts = F,
  reorder = T,
  thresholds = F,
  fixedStyle =1,
  node.height = 1,
  node.width = 4,
  label.scale = F,
  shapeMan = "rectangle",
  shapeLat = "ellipse",
  nodeLabels = nodeNames2,
  details = T)

ld<- standardizedsolution(m4_rmsea_adj.fit) %>% dplyr::select(lhs,op,rhs,est.std) %>% filter(op=="=~") 
ld$est.std<- sprintf("%.2f", ld$est.std)

#pegar cargas factoriales al plot
text(x = -0.1,y =  1.00, ld$est.std[1],font = 5,cex = 1.20)
text(x = -0.1,y =  0.76, ld$est.std[2],font = 5,cex = 1.20)
text(x = -0.1,y =  0.19, ld$est.std[3],font = 5,cex = 1.20)
text(x = -0.1,y =  -0.04, ld$est.std[4],font = 5,cex = 1.20)
text(x = -0.1,y = -0.6, ld$est.std[5],font = 5,cex = 1.20)
text(x = -0.1,y = -0.85, ld$est.std[6],font = 5,cex = 1.20)

text(x = 1.58,y = 0, "0.4",font = 5,cex = 1.20)

library(draw) #para hacer rectangulo de fit

adj_y <- 0.6
adj_x <- 0.2
drawBox(x = 3.5, y = 1.2-adj_y, width = 7.5, height = 2,fillColor = "grey",opacity = 0)# marca espacio
drawBox(x = 3.5, y = 1.4-adj_y, width = 5.5, height = 1,fillColor = "grey",opacity = 0.5) # crea rectangulo

# paste0("Estimator DWLS", ", N=",nobs(m4_rmsea_adj.fit))
# paste0("Model fit: ", "χ²","(",fm333[2,],")=",fm333[1,],"***","; CFI=",fm333[3,],"; RMSEA=",fm333[4,])
# paste0("***p<0.001")

#pegar texto fit en rectangulo

size <- 10

drawText(x = 1.4+adj_x, y = 1.75-adj_y, text = paste0("Estimator: DWLS", ", N=",nobs(m4_rmsea_adj.fit)),family = "serif",size = size)
drawText(x = 2.3+adj_x, y = 1.59-adj_y, text = paste0("Model fit: ", "χ²","(",fm333[2,],")=",fm333[1,],"***","; CFI=",fm333[3,],"; RMSEA=",fm333[4,]),family = "serif",size = size)
drawText(x = 1.0+adj_x, y = 1.43-adj_y, text = paste0("***p<0.001"),family = "serif",size = size)


#guardar en png
drawExport("Output/images/merit_cfa_nopref_path.png",units = "cm",width = 30,height = 20,ppi = 300)
