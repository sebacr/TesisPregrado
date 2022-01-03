
library(sjPlot)
library(dplyr)
library(lavaan)
library(semPlot)
library(stargazer)
library(corrplot)
library(psych)
library(knitr)
library(kableExtra)
library(rvest)
library(sjlabelled)
library(ggplot2)

load(file = "D:/Documentos/Sociologia 9/proyectos reproducibles/merit-scale-master/input/data/proc/dat01.RData")
load(file = "D:/Documentos/Sociologia 9/proyectos reproducibles/merit-scale-master/input/data/proc/dat02.RData")
load(file = "D:/Documentos/Sociologia 9/proyectos reproducibles/merit-scale-master/input/data/proc/dat03.RData")
load(file = "D:/Documentos/Sociologia 9/proyectos reproducibles/merit-scale-master/input/data/proc/dat04.RData")

data01 <- sjlabelled::read_spss(path = "D:/Documentos/Sociologia 9/proyectos reproducibles/merit-scale-master/input/data/original/Estudio_3_ola1.sav",verbose = FALSE)
dat01 <- data01 %>% filter(Intro==1,Finished==1) %>% dplyr::select(starts_with("meritv01"))   %>% na.omit()
dat02 <- data01 %>% filter(Intro==1,Finished==1) %>% dplyr::select(starts_with("meritv02"))   %>% na.omit()
dat03 <- data01 %>% filter(Intro==1,Finished==1) %>% dplyr::select(starts_with("meritv03_p")) %>% na.omit()

# - Nos quedamos solamente con los casos que ACEPTARON participar y terminaron el cuestionario.
# - Para dejar una base que contenga solamente los casos que tienen información completa en los ítems usamos na.omit()

dat04 <- data01 %>% filter(Intro==1) %>%
  dplyr::select(starts_with("meritv01"),
                starts_with("meritv02"),
                starts_with("meritv03_p"),
                starts_with("FL_21_DO"))
dat04$grupo <- NA
dat04$grupo[dat04$FL_21_DO_merit_perc_pref_julio19v01==1] <- 1
dat04$grupo[dat04$FL_21_DO_merit_perc_pref_julio19v02==1] <- 2
dat04$grupo[dat04$FL_21_DO_merit_perc_pref_julio19v03==1] <- 3
dat04$perc_effort <- rowSums(dat04[,c(matches(match = "perc_effort",vars = names(dat04)))],na.rm = TRUE)
dat04$perc_talent <- rowSums(dat04[,c(matches(match = "perc_talent",vars = names(dat04)))],na.rm = TRUE)
dat04$perc_wpart  <- rowSums(dat04[,c(matches(match = "perc_wpart" ,vars = names(dat04)))],na.rm = TRUE)
dat04$perc_netw   <- rowSums(dat04[,c(matches(match = "perc_netw"  ,vars = names(dat04)))],na.rm = TRUE)

dat04$pref_effort <- rowSums(dat04[,c(matches(match = "pref_effort",vars = names(dat04)))],na.rm = TRUE)
dat04$pref_talent <- rowSums(dat04[,c(matches(match = "pref_talent",vars = names(dat04)))],na.rm = TRUE)
dat04$pref_wpart  <- rowSums(dat04[,c(matches(match = "pref_wpart" ,vars = names(dat04)))],na.rm = TRUE)
dat04$pref_netw   <- rowSums(dat04[,c(matches(match = "pref_netw"  ,vars = names(dat04)))],na.rm = TRUE)
dat04[dat04==0] <- NA
dat04 <- dat04 %>% dplyr::select(starts_with(match = "perc_"),starts_with(match = "pref_"),grupo) %>% na.omit()


model01 <- '
perc_merit=~meritv01_perc_effort +meritv01_perc_talent 
perc_nmerit=~meritv01_perc_wpart  +meritv01_perc_netw   
pref_merit=~meritv01_pref_effort +meritv01_pref_talent 
pref_nmerit=~meritv01_pref_wpart  +meritv01_pref_netw   
'

fit1_c <- cfa(model = model01,data = dat01,estimator="MLR") # Continuous/ estimator ML Robust

fit1_o <- cfa(model = model01,data = dat01,
              ordered = c("meritv01_perc_effort","meritv01_perc_talent",
                          "meritv01_perc_wpart","meritv01_perc_netw",
                          "meritv01_pref_effort","meritv01_pref_talent",
                          "meritv01_pref_wpart","meritv01_pref_netw"))

summary(fit1_c,fit.measures=TRUE,standardized=TRUE,rsquare=T)

#---------------------------------------------------#
cnames <- c("Factor","Indicator","Loading (MLR)","Loading (DWLS)")
kable(left_join(x = standardizedsolution(fit1_c) %>% filter(op=="=~") %>% dplyr::select(lhs,rhs,est.std),y = standardizedsolution(fit1_o) %>% filter(op=="=~") %>% dplyr::select(lhs,rhs,est.std),c("lhs","rhs")),
      format = "markdown",digits = 2,col.names = cnames, caption = "Factor loadings")




model02 <- '
perc_merit=~meritv02_perc_effort +meritv02_perc_talent 
perc_nmerit=~meritv02_perc_wpart  +meritv02_perc_netw   
pref_merit=~meritv02_pref_effort +meritv02_pref_talent 
pref_nmerit=~meritv02_pref_wpart  +meritv02_pref_netw
'

fit2_c <- cfa(model = model02,data = dat02,estimator="MLR") # Continuous/ estimator ML Robust

fit2_o <- cfa(model = model02,data = dat02,ordered = c("meritv02_perc_effort","meritv02_perc_talent",
                                                       "meritv02_perc_wpart","meritv02_perc_netw",
                                                       "meritv02_pref_effort","meritv02_pref_talent",
                                                       "meritv02_pref_wpart","meritv02_pref_netw"))

summary(fit2_c,fit.measures=TRUE,standardized=TRUE,rsquare=T)


#---------------------------------------------------#
cnames <- c("Factor","Indicator","Loading (MLR)","Loading (DWLS)")
kable(left_join(x = standardizedsolution(fit2_c) %>% filter(op=="=~") %>% dplyr::select(lhs,rhs,est.std),y = standardizedsolution(fit2_o) %>% filter(op=="=~") %>% dplyr::select(lhs,rhs,est.std),c("lhs","rhs")),
      format = "markdown",digits = 2,col.names = cnames, caption = "Factor loadings")









model03 <- '
perc_merit=~meritv03_perc_effort +meritv03_perc_talent 
perc_nmerit=~meritv03_perc_wpart  +meritv03_perc_netw   
pref_merit=~meritv03_pref_effort +meritv03_pref_talent 
pref_nmerit=~meritv03_pref_wpart  +meritv03_pref_netw
'

fit3_c <- cfa(model = model03,data = dat03,estimator="MLR",std.lv=FALSE) # Continuous/ estimator ML Robust

fit3_o <- cfa(model = model03,data = dat03,ordered = c("meritv03_perc_effort","meritv03_perc_talent",
                                                       "meritv03_perc_wpart","meritv03_perc_netw",
                                                       "meritv03_pref_effort","meritv03_pref_talent",
                                                       "meritv03_pref_wpart","meritv03_pref_netw"),std.lv=FALSE)

summary(fit3_o,fit.measures=TRUE,standardized=TRUE,rsquare=T)


#---------------------------------------------------#
cnames <- c("Factor","Indicator","Loading (MLR)","Loading (DWLS)")
kable(left_join(x = standardizedsolution(fit3_c) %>% filter(op=="=~") %>% select(lhs,rhs,est.std),y = standardizedsolution(fit3_o) %>% filter(op=="=~") %>% select(lhs,rhs,est.std),c("lhs","rhs")),
      format = "markdown",digits = 2,col.names = cnames, caption = "Factor loadings")


# tau test

model03.tau <- '
perc_merit=~r1*meritv03_perc_effort +r1*meritv03_perc_talent 
perc_nmerit=~r2*meritv03_perc_wpart  +r2*meritv03_perc_netw   
pref_merit=~r3*meritv03_pref_effort +r3*meritv03_pref_talent 
pref_nmerit=~r4*meritv03_pref_wpart  +r4*meritv03_pref_netw
'

fit3_c.tau <- cfa(model = model03.tau,data = dat03,estimator="MLR",std.lv=FALSE) # Continuous/ estimator ML Robust

fit3_o.tau <- cfa(model = model03.tau,data = dat03,ordered = c("meritv03_perc_effort","meritv03_perc_talent",
                                                               "meritv03_perc_wpart","meritv03_perc_netw",
                                                               "meritv03_pref_effort","meritv03_pref_talent",
                                                               "meritv03_pref_wpart","meritv03_pref_netw"), std.lv=FALSE)


summary(fit3_o.tau,fit.measures=TRUE,standardized=TRUE,rsquare=T)

#---------------------------------------------------#
cnames <- c("Factor","Indicator","Loading (MLR)","Loading (DWLS)")
kable(left_join(x = standardizedsolution(fit3_c.tau ) %>% filter(op=="=~") %>% select(lhs,rhs,est.std),y = standardizedsolution(fit3_o.tau) %>% filter(op=="=~") %>% select(lhs,rhs,est.std),c("lhs","rhs")),
      format = "markdown",digits = 2,col.names = cnames)






kable(rbind(Ordinal    =fitmeasures(fit3_o    )[c("chisq.scaled","df","cfi","cfi.scaled","cfi.robust","rmsea","rmsea.scaled","rmsea.robust")],
            Ordinal.tau=fitmeasures(fit3_o.tau)[c("chisq.scaled","df","cfi","cfi.scaled","cfi.robust","rmsea","rmsea.scaled","rmsea.robust")]),
      format = "markdown",
      digits = 3, caption = "Fit tau-equivalence")
kable(anova(fit3_o,fit3_o.tau),
      format = "markdown",
      digits = 3)









model04 <- '
perc_merit =~ perc_effort+perc_talent
perc_nmerit=~perc_wpart +perc_netw
pref_merit =~ pref_effort+pref_talent
pref_nmerit=~pref_wpart +pref_netw'

fit4_c <- cfa(model = model04,data = dat04,estimator="MLR",std.lv=FALSE) # Continuous/ estimator ML Robust

fit4_o <- cfa(model = model04,data = dat04,ordered = c("perc_effort","perc_talent",
                                                       "perc_wpart","perc_netw",
                                                       "pref_effort","pref_talent",
                                                       "pref_wpart","pref_netw"),std.lv=FALSE)
#---------------------------------------------------#
cnames <- c("Factor","Indicator","Loading (MLR)","Loading (DWLS)")
kable(left_join(x = standardizedsolution(fit4_c ) %>% filter(op=="=~") %>% select(lhs,rhs,est.std),y = standardizedsolution(fit4_o) %>% filter(op=="=~") %>% select(lhs,rhs,est.std),c("lhs","rhs")),
      format = "markdown",digits = 2,col.names = cnames, caption = "Factor loadings")




nodeNames <-c("Perception effort","Perception talent","Perception rich family","Perception contacts",
              "Preference effort","Preference talent","Preference rich family","Preference contacts",
              "Perception meritocratic","Perception non meritocratic","Preference meritocratic","Preference non meritocratic")

semPaths(
  fit4_o ,
  whatLabels = "std",
  # what = "col",
  label.cex = 1,
  edge.label.cex = 0.7,
  residuals = F,
  optimizeLatRes =T,
  edge.color = "black",
  style = "lisrel",
  fade = F,
  nCharNodes = 0,
  curvePivot = FALSE,
  curve = 3,
  rotation = 4,
  layout = "tree2",
  cardinal = "lat cov",
  legend.cex = 0.6,
  label.cex = 1,
  label.font = 6,
  edge.label.font = 7,
  asize = 3,
  edge.width = 1,
  sizeMan = 5,
  sizeLat = 10,
  width = 22, 
  height = 15,
  # nodeNames = nodeNames,
  intercepts = F, 
  reorder = F,
  thresholds = F,
  fixedStyle =1,
  nodeLabels = c("A", "B", "C", "D", "E", "F", "G", "H", "1", "2", "3", "4")
)






nodeNames <-c("Perception effort","Perception talent","Perception rich family","Perception contacts",
              "Preference effort","Preference talent","Preference rich family","Preference contacts",
              "Perception \n meritocratic","Perception \n non meritocratic","Preference \n meritocratic","Preference \n non meritocratic")

# graphics.off()
fm03<- data.frame(v1=fitmeasures(fit4_o, output ="matrix")[c("chisq.scaled","df","cfi.scaled","rmsea.scaled"),])
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
  fit4_o ,
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

ld<- standardizedsolution(fit4_o) %>% select(lhs,op,rhs,est.std) %>% filter(op=="=~") 
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
#install.packages("draw")
library(draw) #para hacer rectangulo de fit

adj_y <- 0.6
adj_x <- 0.2
drawBox(x = 3.5, y = 1.2-adj_y, width = 7.5, height = 2,fillColor = "grey",opacity = 0)# marca espacio
drawBox(x = 3.5, y = 1.4-adj_y, width = 5.5, height = 1,fillColor = "grey",opacity = 0.5) # crea rectangulo

# paste0("Estimator DWLS", ", N=",nobs(fit4_o))
# paste0("Model fit: ", "χ²","(",fm03[2,],")=",fm03[1,],"***","; CFI=",fm03[3,],"; RMSEA=",fm03[4,])
# paste0("***p<0.001")

#pegar texto fit en rectangulo

size <- 10

drawText(x = 1.4+adj_x, y = 1.75-adj_y, text = paste0("Estimator: DWLS", ", N=",nobs(fit4_o)),family = "serif",size = size)
drawText(x = 2.3+adj_x, y = 1.59-adj_y, text = paste0("Model fit: ", "χ²","(",fm03[2,],")=",fm03[1,],"***","; CFI=",fm03[3,],"; RMSEA=",fm03[4,]),family = "serif",size = size)
drawText(x = 1.0+adj_x, y = 1.43-adj_y, text = paste0("***p<0.001"),family = "serif",size = size)











latcor<- lavInspect(fit4_o,what = "cor.lv")
windowsFonts(A = windowsFont("Times New Roman"))
rownames(latcor) <-c(
  "A. Meritocratic Perception",
  "B. Non-Meritocratic Perception",
  "C. Meritocratic Preferences",
  "D. Non-Meritocratic Preferences"
)
colnames(latcor) <-c("(A)", "(B)","(C)","(D)")
latcor[latcor==1] <- NA
png("D:/Documentos/Sociologia 9/proyectos reproducibles/merit-scale-master/output/images/latcor.png",
    width = 18,height = 13,units="cm",
    pointsize=12,bg="white",res=300)
corrplot(
  latcor,
  method = "color",
  type = "upper",
  tl.col = "black",
  addCoef.col = "black",
  diag = TRUE,
  family = "A",
  na.label = "-",
  number.font = 6,
  tl.cex =0.75,
  number.cex = 1)
dev.off()











# TABLAS 


tb.load<- data.frame(round(cbind(inspect(fit1_o,what="std")$lambda,
                                 inspect(fit2_o,what="std")$lambda,
                                 inspect(fit3_o,what="std")$lambda),
                           digits = 2))
tb.load[tb.load==c(0.00)] <- NA; names(tb.load) <- c(paste0("M1.",1:4),paste0("M2.",1:4),paste0("M3.",1:4))

for (i in names(tb.load)) {
  # tb.load[,i] <- sjlabelled::as_character(tb.load[,i])
  tb.load[,i] <- sprintf(tb.load[,i], fmt = '%#.2f')
}
tb.load[tb.load=="NA"] <- ""

#-------#
fm01<- data.frame(t(data.frame(fitmeasures(fit1_o, output ="matrix")[c("chisq","df","cfi","tli","rmsea"),]))); row.names(fm01) ="M1.1"
fm02<- data.frame(t(data.frame(fitmeasures(fit2_o, output ="matrix")[c("chisq","df","cfi","tli","rmsea"),]))); row.names(fm02) ="M2.1"
fm03<- data.frame(t(data.frame(fitmeasures(fit3_o, output ="matrix")[c("chisq","df","cfi","tli","rmsea"),]))); row.names(fm03) ="M3.1"

#------chi2, df------#
fm04<- round(rbind(fm01,fm02,fm03),3)
fm04.1 <- fm04 %>% select(chisq,df) 
fm04.1$chisq <- round(x = fm04.1$chisq,digits = 1)
fm04.1$df <- round(x = fm04.1$df,digits = 0)
fm04.1$chi2df <- paste0(fm04.1$chisq,"(",fm04.1$df,")")
fm04.1 <- select(fm04.1,"chi2df")
for (i in names(fm04.1)) {
  fm04.1[,i] <- as.character(fm04.1[,i])
}

#------CFI, RMSEA------#
fm04.2 <- fm04 %>% select(cfi,tli,rmsea) 
for (i in names(fm04.2)) {
  fm04.2[,i] <- sprintf(fm04.2[,i], fmt = '%#.3f')
}

fm.df      <- bind_cols(fm04.1,fm04.2)
fm.df$nobs <- c(nobs(fit1_o),nobs(fit2_o),nobs(fit3_o)) 
fm.df <- data.frame(t(fm.df)); colnames(fm.df) <- c("M1.2","M2.2","M3.2")

?labs
#------ merge ------#
tb.fm<- bind_rows(tb.load,fm.df)
tb.fm<- tb.fm %>% mutate(vars=c(labs,"$\\chi^2\\text{(df)}$","$\\text{CFI}$","$\\text{TLI}$","$\\text{RMSEA}$","$N$")) %>% dplyr::select(vars,everything())
#--table---#
tb.col  <- c("Variables",1:4,1:4,1:4)
tb.foot <- paste0("; p<0.05=$*$; p<0.01=$**$; p<0.001=$***$. Standardised factor loadings using DWLS estimator. CFI = Comparative fit index (scaled), RMSEA = Root mean square error of approximation (scaled)")
tb.caption <- "Factor loadings and fit measures" 

tb.fm01<- kable(tb.fm,escape = FALSE,align = "lcccccccccccc",col.names = tb.col, caption = tb.caption) %>% 
  kable_styling(full_width = F,font_size = 9) %>% 
  # column_spec(column = 1,width = "10cm",width_min = "12cm",include_thead = TRUE) %>% 
  # column_spec(column = 2:13,width = "1cm",width_min = "1.3cm",include_thead = TRUE) %>% 
  add_header_above(header = c(" "=1,"Version 1"= 4,"Version 2"= 4,"Version 3"= 4)) %>% 
  add_header_above(header = c(" "=1,"Factor loadings"= 12)) %>% 
  row_spec(row = 8,hline_after = TRUE) %>%
  add_indent(c(9:12)) %>% 
  footnote(general =tb.foot ,footnote_as_chunk = T);tb.fm01
save(tb.fm01,file = "output/tables/tb.fm01.RData")


save(tb.fm,file = "output/tables/tb.fm.RData")
save(fm.df,file = "output/tables/fm.df.RData")
save(tb.load,file = "output/tables/tb.load.RData")











sum_fit<- bind_rows(fitmeasures(fit1_c)[c("chisq.scaled","df","cfi","cfi.scaled","cfi.robust","rmsea","rmsea.scaled","rmsea.robust")],
                    fitmeasures(fit1_o)[c("chisq.scaled","df","cfi","cfi.scaled","cfi.robust","rmsea","rmsea.scaled","rmsea.robust")],
                    fitmeasures(fit2_c)[c("chisq.scaled","df","cfi","cfi.scaled","cfi.robust","rmsea","rmsea.scaled","rmsea.robust")],
                    fitmeasures(fit2_o)[c("chisq.scaled","df","cfi","cfi.scaled","cfi.robust","rmsea","rmsea.scaled","rmsea.robust")],
                    fitmeasures(fit3_c)[c("chisq.scaled","df","cfi","cfi.scaled","cfi.robust","rmsea","rmsea.scaled","rmsea.robust")],
                    fitmeasures(fit3_o)[c("chisq.scaled","df","cfi","cfi.scaled","cfi.robust","rmsea","rmsea.scaled","rmsea.robust")],
                    fitmeasures(fit4_c)[c("chisq.scaled","df","cfi","cfi.scaled","cfi.robust","rmsea","rmsea.scaled","rmsea.robust")],
                    fitmeasures(fit4_o)[c("chisq.scaled","df","cfi","cfi.scaled","cfi.robust","rmsea","rmsea.scaled","rmsea.robust")])
sum_fit$mod <- c("Model 1","Model 1","Model 2","Model 2","Model 3","Model 3","Model 4","Model 4")
sum_fit$est <- c("MLR","DWLS","MLR","DWLS","MLR","DWLS","MLR","DWLS")
sum_fit <- select(sum_fit,mod,est,everything())
colnames <- c("Model","Estimator","$\\chi^2$","df","CFI","CFI.sca","CFI.rob","RMSEA","RMSEA.sca","RMSEA.rob")


sumtable01<- kable(sum_fit,digits = 3,format = "html",row.names = F,booktabs=T, caption = "Summary fit indices wave 01",col.names = colnames,escape = FALSE) %>%
  kable_styling(full_width = F)  %>%
  collapse_rows(columns = 1,valign = "middle")  %>%
  footnote(number = c("Model 1: fixed order by percepction/preference",
                      "Model 2: fixed order by topic (i.e: effort)",
                      "Model 3: Randomized order",
                      "Model 4: Complete sample"));sumtable01









############# LA TABLA QUE SIRVE ##########################


sum_fit<- bind_rows(fitmeasures(fit4_o)[c("chisq.scaled","df","cfi.scaled","rmsea","rmsea.scaled")],
                    fitmeasures(fit1_o)[c("chisq.scaled","df","cfi.scaled","rmsea","rmsea.scaled")],
                    fitmeasures(fit2_o)[c("chisq.scaled","df","cfi.scaled","rmsea","rmsea.scaled")],
                    fitmeasures(fit3_o)[c("chisq.scaled","df","cfi.scaled","rmsea","rmsea.scaled")])
sum_fit$mod <- c("Model 1","Model 2","Model 3","Model 4")
sum_fit$est <- c("DWLS","DWLS","DWLS","DWLS")
sum_fit <- select(sum_fit,mod,est,everything())
colnames <- c("Model","Estimator","$\\chi^2$","df","CFI.sca","RMSEA","RMSEA.sca")


sumtable02<- kable(sum_fit,digits = 3,format = "html",row.names = F,booktabs=T, caption = "Summary fit indices",col.names = colnames,escape = FALSE) %>%
  kable_styling(full_width = F)  %>%
  collapse_rows(columns = 1,valign = "middle")  %>%
  footnote(number = c("Model 1: Complete sample",
                      "Model 2: fixed order by percepction/preference",
                      "Model 3:  fixed order by topic (i.e: effort)",
                      "Model 4: Randomized order"));sumtable02












model04 <- '
perc_merit =~ perc_effort + perc_talent
perc_nmerit=~ perc_wpart  + perc_netw
pref_merit =~ pref_effort + pref_talent
pref_nmerit=~ pref_wpart  + pref_netw'

fit4_c_mg <- cfa(model = model04,data = dat04,estimator="MLR",std.lv=FALSE,group = "grupo") # Continuous/ estimator ML Robust
fit4_o_mg <- cfa(model = model04,data = dat04,ordered = c("perc_effort","perc_talent",
                                                          "perc_wpart","perc_netw",
                                                          "pref_effort","pref_talent",
                                                          "pref_wpart","pref_netw"),std.lv=FALSE, group = "grupo")
#---------------------------------------------------#
kable(rbind(Continuous=fitmeasures(fit4_c_mg)[c("chisq.scaled","df","cfi","cfi.scaled","cfi.robust","rmsea","rmsea.scaled","rmsea.robust")],
            Ordinal=  fitmeasures(fit4_o_mg)[c("chisq.scaled","df","cfi","cfi.scaled","cfi.robust","rmsea","rmsea.scaled","rmsea.robust")]),
      format = "markdown",digits = 3,caption = "Fit indices")
gr01=standardizedsolution(fit4_c_mg) %>% filter(op=="=~") %>% select(group,lhs,rhs,est.std)
gr02=standardizedsolution(fit4_o_mg) %>% filter(op=="=~") %>% select(group,lhs,rhs,est.std)
gr01$group=dplyr::recode(gr01$group, `1` = "mod03", `2` = "mod02", `3` = "mod01")
gr02$group=dplyr::recode(gr02$group, `1` = "mod03", `2` = "mod02", `3` = "mod01")

cnames <- c("Grupo","Factor","Indicator","Loading (MLR)","Loading (DWLS)")
kable(left_join(x =gr01 ,
                y =gr02 ,
                c("group","lhs","rhs")),
      format = "markdown",digits = 2,col.names = cnames,caption = "Factor loadings by group")
















################# Gráfico util para ver distribución de var dep e indep 


library(ggplot2)
library(reshape2)
library(sjPlot)
library(dplyr)

#install.packages("cowplot")
library(cowplot)

dat_merit <- dat04 %>% select("a.Effort"=perc_effort,"a.Talent"=perc_talent, "a.Rich Parents"=perc_wpart, "a.Contacts"=perc_netw, "b.Effort"=pref_effort,"b.Talent"=pref_talent,"b.Rich Parents"=pref_wpart, "b.Contacts"=pref_netw)

dat_merit_2<- dat_merit %>% sjmisc::rec(rec="rev") %>% 
  select(
    "a.Effort" = a.Effort_r,
    "a.Talent" = a.Talent_r,
    "a.Rich Parents" = "a.Rich Parents_r",
    "a.Contacts" = a.Contacts_r,
    "b.Effort" = b.Effort_r,
    "b.Talent" = b.Talent_r,
    "b.Rich Parents" = "b.Rich Parents_r",
    "b.Contacts" = b.Contacts_r)


plotlikert<-plot_likert(dat_merit_2, 
                        c(1, 1, 1, 1, 2, 2, 2, 2),
                        groups.titles = c("Perceptions", "Preferences"),
                        geom.colors = "PuBu",
                        geom.size = 0.8,
                        axis.labels = c("Effort", "Talent", "Rich Parents", "Contacts"),
                        catcount = 4,
                        cat.neutral = 3,
                        grid.range  =  c (1.2 , 1.4),
                        values  =  "sum.outside", 
                        reverse.colors = T,
                        reverse.scale = F)

plotlikert

ggsave(plotlikert,filename = "output/images/plotlikert.png",device = "png",width = 30,height = 15,dpi = "retina",units = "cm")






datli03 <- dat04 %>% select(starts_with("perc_"),
                            starts_with("pref_")) %>%  sjmisc::rec(rec="rev",append = F)

datli03 <- remove_label(datli03)

set_theme(base = theme_bw(),
          theme.font = "serif", 
          axis.textsize.x = 1.2,
          axis.textsize.y = 1.2, 
          geom.label.size = 4, 
          legend.just = 0.35, 
          legend.size = 1.2,
          legend.backgroundcol = "white")
# http://www.sthda.com/english/wiki/colors-in-r  = PALETAS de COLORES 
plot01<- plot_likert(
  datli03,
  c(rep("Perception", 4), rep("Preference", 4)),
  values  =  "sum.outside",
  show.prc.sign = F,
  sort.groups = FALSE,
  grid.range = c(1.0, 1.0),
  geom.colors = "Blues",
  rel_heights = c(4, 4),
  wrap.labels = 100,
  reverse.scale = FALSE,
  cat.neutral = 3, 
  legend.pos = "bottom", 
  group.legend.options = list(reverse = TRUE),
  axis.labels = c("Effort", "Talent", "Rich parents", "Contacts"))
plot01

ggsave(plot01,filename = "output/images/plotlikert.png",device = "png",width = 40,height = 25,dpi = "retina",units = "cm")





