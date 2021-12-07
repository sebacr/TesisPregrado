pacman::p_load(dplyr, sjmisc, car, sjlabelled,labelled, stargazer,kableExtra,corrplot,sessioninfo,readxl,
               pander,xtable, tidyverse, extrafont, ggplot2, forcats, ggpubr, naniar,haven, devtools,summarytools,
               poLCA,sjPlot,lavaan,semTable,tidySEM,lavaanPlot,semPlot,psych,diagram,texreg)
#install.packages("webshot",dependencies = TRUE)
library(webshot)
webshot::install_phantomjs(force = TRUE)
devtools::install_github("dr-JT/semoutput")

load("Input/Data_proc/data.RData") 



ajuste <- c("\\textbf{$\\chi^2$}","\\textbf{gl}","\\textbf{CFI}","\\textbf{RMSEA}","\\textbf{Num. obs.}")

# Structural Regression Model

################################ Modelo 1 (variables objetivas) ###################################

m.obj <- '
# measurement model
## Endogenous factors 
Atribuciones_Internas=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
Atribuciones_Externas=~atrib_pob_4+atrib_pob_5+atrib_riq_4

# Cov
atrib_riq_1 ~~ atrib_riq_3

# regressions

Atribuciones_Internas ~ sexo + edad + educ_tecnica + educ_uni_incomp + educ_uni_post + 
ingresos_1 + ingresos_2 + ingresos_3 + ingresos_4 +
act_prin_no_remunerado + act_prin_desempleado + act_prin_trabaja_estudia + act_prin_estudia + act_prin_jubliado
Atribuciones_Externas ~ sexo + edad + educ_tecnica + educ_uni_incomp + educ_uni_post + 
ingresos_1 + ingresos_2 + ingresos_3 + ingresos_4 +
act_prin_no_remunerado + act_prin_desempleado + act_prin_trabaja_estudia + act_prin_estudia + act_prin_jubliado
'
fit_m.obj <- sem(m.obj, data=data,ordered = T)
#semoutput::sem_paths(fit_m.obj, standardized = TRUE)

x <-  parameterEstimates(fit_m.obj) #Para saber la posición de las filas
options(max.print=999999)
x
int.obj <- as.data.frame(parameterEstimates(fit_m.obj)[9:22,  ])
ext.obj <- as.data.frame(parameterEstimates(fit_m.obj)[23:36,  ])
fit.messem.inc <- fitMeasures(fit_m.obj,c("chisq","df","pvalue","cfi","rmsea"))
#----Crear la tabla-----
m.int.obj <- texreg::createTexreg(coef.names  = int.obj$rhs ,
                                coef = int.obj$est,
                                se = int.obj$z,
                                pvalues = int.obj$pvalue,
                                gof = c(fitmeasures(fit_m.obj)[[c(3)]],
                                        fitmeasures(fit_m.obj)[[c(4)]],
                                        fitmeasures(fit_m.obj)[[c(18)]],
                                        fitmeasures(fit_m.obj)[[c(36)]],
                                        nobs(fit_m.obj)),
                                gof.names = ajuste,
                                gof.decimal =c (TRUE, FALSE,TRUE, TRUE, FALSE))
m.ext.obj <- texreg::createTexreg(coef.names  = ext.obj$rhs ,
                                  coef = ext.obj$est,
                                  se = ext.obj$z,
                                  pvalues = ext.obj$pvalue,
                                  gof = c(fitmeasures(fit_m.obj)[[c(3)]],
                                          fitmeasures(fit_m.obj)[[c(4)]],
                                          fitmeasures(fit_m.obj)[[c(18)]],
                                          fitmeasures(fit_m.obj)[[c(36)]],
                                          nobs(fit_m.obj)),
                                  gof.names = ajuste,
                                  gof.decimal =c (TRUE, FALSE,TRUE, TRUE, FALSE))


################################ Modelo 2 (variable subjetiva) ###################################

m.subj <- '
# measurement model
## Endogenous factors 
Atribuciones_Internas=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
Atribuciones_Externas=~atrib_pob_4+atrib_pob_5+atrib_riq_4

# Cov
atrib_riq_1 ~~ atrib_riq_3

# regressions

Atribuciones_Internas ~ ess_ind 
Atribuciones_Externas ~ ess_ind
'
fit_m.subj <- sem(m.subj, data=data,ordered = T)
semoutput::sem_paths(fit_m.subj, standardized = TRUE, ci = "standardized")
x <-  parameterEstimates(fit_m.subj) #Para saber la posición de las filas
x
int.subj <- as.data.frame(parameterEstimates(fit_m.subj)[9,  ])
ext.subj <- as.data.frame(parameterEstimates(fit_m.subj)[10,  ])
fit.messem.inc <- fitMeasures(fit_m.subj,c("chisq","df","pvalue","cfi","rmsea"))
#----Crear la tabla-----
m.int.subj <- texreg::createTexreg(coef.names  = int.subj$rhs ,
                                  coef = int.subj$est,
                                  se = int.subj$z,
                                  pvalues = int.subj$pvalue,
                                  gof = c(fitmeasures(fit_m.subj)[[c(3)]],
                                          fitmeasures(fit_m.subj)[[c(4)]],
                                          fitmeasures(fit_m.subj)[[c(18)]],
                                          fitmeasures(fit_m.subj)[[c(36)]],
                                          nobs(fit_m.subj)),
                                  gof.names = ajuste,
                                  gof.decimal =c (TRUE, FALSE,TRUE, TRUE, FALSE))
m.ext.subj <- texreg::createTexreg(coef.names  = ext.subj$rhs ,
                                   coef = ext.subj$est,
                                   se = ext.subj$z,
                                   pvalues = ext.subj$pvalue,
                                   gof = c(fitmeasures(fit_m.subj)[[c(3)]],
                                           fitmeasures(fit_m.subj)[[c(4)]],
                                           fitmeasures(fit_m.subj)[[c(18)]],
                                           fitmeasures(fit_m.subj)[[c(36)]],
                                           nobs(fit_m.subj)),
                                   gof.names = ajuste,
                                   gof.decimal =c (TRUE, FALSE,TRUE, TRUE, FALSE))

############################# Modelo 3 (variables objetivas y subjetiva) ##############################

m.obj.subj <- '
# measurement model
## Endogenous factors 
Atribuciones_Internas=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
Atribuciones_Externas=~atrib_pob_4+atrib_pob_5+atrib_riq_4

# Cov
atrib_riq_1 ~~ atrib_riq_3

# regressions

Atribuciones_Internas ~ educ_tecnica + educ_uni_incomp + educ_uni_post + 
ingresos_1 + ingresos_2 + ingresos_3 + ingresos_4 +
act_prin_no_remunerado + act_prin_desempleado + act_prin_trabaja_estudia + act_prin_estudia + act_prin_jubliado 
+ ess_ind
Atribuciones_Externas ~ educ_tecnica + educ_uni_incomp + educ_uni_post + 
ingresos_1 + ingresos_2 + ingresos_3 + ingresos_4 +
act_prin_no_remunerado + act_prin_desempleado + act_prin_trabaja_estudia + act_prin_estudia + act_prin_jubliado
+ ess_ind
'
fit_m.obj.subj <- sem(m.obj.subj, data=data,ordered = T)
semoutput::sem_paths(fit_m.obj.subj, standardized = TRUE, ci = "standardized")
x <-  parameterEstimates(fit_m.obj.subj) #Para saber la posición de las filas
x
int.obj.subj <- as.data.frame(parameterEstimates(fit_m.obj.subj)[9:18,  ])
ext.obj.subj <- as.data.frame(parameterEstimates(fit_m.obj.subj)[19:28,  ])
fit.messem.inc <- fitMeasures(fit_m.subj,c("chisq","df","pvalue","cfi","rmsea"))
#----Crear la tabla-----
m.int.obj.subj <- texreg::createTexreg(coef.names  = int.obj.subj$rhs ,
                                   coef = int.obj.subj$est,
                                   se = int.obj.subj$z,
                                   pvalues = int.obj.subj$pvalue,
                                   gof = c(fitmeasures(fit_m.obj.subj)[[c(3)]],
                                           fitmeasures(fit_m.obj.subj)[[c(4)]],
                                           fitmeasures(fit_m.obj.subj)[[c(18)]],
                                           fitmeasures(fit_m.obj.subj)[[c(36)]],
                                           nobs(fit_m.subj)),
                                   gof.names = ajuste,
                                   gof.decimal =c (TRUE, FALSE,TRUE, TRUE, FALSE))
m.ext.obj.subj <- texreg::createTexreg(coef.names  = ext.obj.subj$rhs ,
                                       coef = ext.obj.subj$est,
                                       se = ext.obj.subj$z,
                                       pvalues = ext.obj.subj$pvalue,
                                       gof = c(fitmeasures(fit_m.obj.subj)[[c(3)]],
                                               fitmeasures(fit_m.obj.subj)[[c(4)]],
                                               fitmeasures(fit_m.obj.subj)[[c(18)]],
                                               fitmeasures(fit_m.obj.subj)[[c(36)]],
                                               nobs(fit_m.subj)),
                                       gof.names = ajuste,
                                       gof.decimal =c (TRUE, FALSE,TRUE, TRUE, FALSE))

################################ Modelo 4 (variables meritocráticas) ###################################


m.merit <- '
# measurement model
## Endogenous factors 
Atribuciones_Internas=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
Atribuciones_Externas=~atrib_pob_4+atrib_pob_5+atrib_riq_4

## Exogenous factors
perc_merit=~merit_perc_effort+merit_perc_talent
pref_merit=~merit_pref_talent+merit_pref_effort
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart

# Cov
atrib_riq_1 ~~ atrib_riq_3

# regressions

Atribuciones_Internas ~ perc_merit + pref_merit 
Atribuciones_Externas ~ perc_merit + pref_merit 
'
fit_m.merit <- sem(m.merit, data=data,ordered = T)
semoutput::sem_paths(fit_m.merit, standardized = TRUE, ci = "standardized")
x <-  parameterEstimates(fit_m.merit) #Para saber la posición de las filas
x
int.merit <- as.data.frame(parameterEstimates(fit_m.merit)[17:18,  ])
ext.merit <- as.data.frame(parameterEstimates(fit_m.merit)[19:20,  ])
fit.messem.inc <- fitMeasures(fit_m.merit,c("chisq","df","pvalue","cfi","rmsea"))
#----Crear la tabla-----
m.int.merit <- texreg::createTexreg(coef.names  = int.merit$rhs ,
                                       coef = int.merit$est,
                                       se = int.merit$z,
                                       pvalues = int.merit$pvalue,
                                       gof = c(fitmeasures(fit_m.merit)[[c(3)]],
                                               fitmeasures(fit_m.merit)[[c(4)]],
                                               fitmeasures(fit_m.merit)[[c(18)]],
                                               fitmeasures(fit_m.merit)[[c(36)]],
                                               nobs(fit_m.merit)),
                                       gof.names = ajuste,
                                       gof.decimal =c (TRUE, FALSE,TRUE, TRUE, FALSE))
m.ext.merit <- texreg::createTexreg(coef.names  = ext.merit$rhs ,
                                    coef = ext.merit$est,
                                    se = ext.merit$z,
                                    pvalues = ext.merit$pvalue,
                                    gof = c(fitmeasures(fit_m.merit)[[c(3)]],
                                            fitmeasures(fit_m.merit)[[c(4)]],
                                            fitmeasures(fit_m.merit)[[c(18)]],
                                            fitmeasures(fit_m.merit)[[c(36)]],
                                            nobs(fit_m.merit)),
                                    gof.names = ajuste,
                                    gof.decimal =c (TRUE, FALSE,TRUE, TRUE, FALSE))
################################ Modelo 5 (variables no meritocráticas) ###################################


m.nmerit <- '
# measurement model
## Endogenous factors 
Atribuciones_Internas=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
Atribuciones_Externas=~atrib_pob_4+atrib_pob_5+atrib_riq_4

## Exogenous factors
perc_merit=~merit_perc_effort+merit_perc_talent
pref_merit=~merit_pref_talent+merit_pref_effort
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart

# Cov
atrib_riq_1 ~~ atrib_riq_3

# regressions

Atribuciones_Internas ~ perc_nmerit + pref_nmerit
Atribuciones_Externas ~ perc_nmerit + pref_nmerit
'
fit_m.nmerit <- sem(m.nmerit, data=data,ordered = T)
semoutput::sem_paths(fit_m.nmerit, standardized = TRUE, ci = "standardized")
x <-  parameterEstimates(fit_m.nmerit) #Para saber la posición de las filas
x
int.nmerit <- as.data.frame(parameterEstimates(fit_m.nmerit)[17:18,  ])
ext.nmerit <- as.data.frame(parameterEstimates(fit_m.nmerit)[19:20,  ])
fit.messem.inc <- fitMeasures(fit_m.nmerit,c("chisq","df","pvalue","cfi","rmsea"))
#----Crear la tabla-----
m.int.nmerit <- texreg::createTexreg(coef.names  = int.nmerit$rhs ,
                                    coef = int.nmerit$est,
                                    se = int.nmerit$z,
                                    pvalues = int.nmerit$pvalue,
                                    gof = c(fitmeasures(fit_m.nmerit)[[c(3)]],
                                            fitmeasures(fit_m.nmerit)[[c(4)]],
                                            fitmeasures(fit_m.nmerit)[[c(18)]],
                                            fitmeasures(fit_m.nmerit)[[c(36)]],
                                            nobs(fit_m.nmerit)),
                                    gof.names = ajuste,
                                    gof.decimal =c (TRUE, FALSE,TRUE, TRUE, FALSE))
m.ext.nmerit <- texreg::createTexreg(coef.names  = ext.nmerit$rhs ,
                                     coef = ext.nmerit$est,
                                     se = ext.nmerit$z,
                                     pvalues = ext.nmerit$pvalue,
                                     gof = c(fitmeasures(fit_m.nmerit)[[c(3)]],
                                             fitmeasures(fit_m.nmerit)[[c(4)]],
                                             fitmeasures(fit_m.nmerit)[[c(18)]],
                                             fitmeasures(fit_m.nmerit)[[c(36)]],
                                             nobs(fit_m.nmerit)),
                                     gof.names = ajuste,
                                     gof.decimal =c (TRUE, FALSE,TRUE, TRUE, FALSE))

###################### Modelo 6 (variables meritocráticas y no meritocráticas) #########################


m.merit.nmerit <- '
# measurement model
## Endogenous factors 
Atribuciones_Internas=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
Atribuciones_Externas=~atrib_pob_4+atrib_pob_5+atrib_riq_4

## Exogenous factors
perc_merit=~merit_perc_effort+merit_perc_talent
pref_merit=~merit_pref_talent+merit_pref_effort
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart

# Cov
atrib_riq_1 ~~ atrib_riq_3

# regressions

Atribuciones_Internas ~ perc_merit + pref_merit + perc_nmerit + pref_nmerit
Atribuciones_Externas ~ perc_merit + pref_merit + perc_nmerit + pref_nmerit
'
fit_m.merit.nmerit <- sem(m.merit.nmerit, data=data,ordered = T)
semoutput::sem_paths(fit_m.merit.nmerit, standardized = TRUE, ci = "standardized")
x <-  parameterEstimates(fit_m.merit.nmerit) #Para saber la posición de las filas
x
int.merit.nmerit <- as.data.frame(parameterEstimates(fit_m.merit.nmerit)[17:20,  ])
ext.merit.nmerit <- as.data.frame(parameterEstimates(fit_m.merit.nmerit)[21:24,  ])
fit.messem.inc <- fitMeasures(fit_m.merit.nmerit,c("chisq","df","pvalue","cfi","rmsea"))
#----Crear la tabla-----
m.int.merit.nmerit <- texreg::createTexreg(coef.names  = int.merit.nmerit$rhs ,
                                     coef = int.merit.nmerit$est,
                                     se = int.merit.nmerit$z,
                                     pvalues = int.merit.nmerit$pvalue,
                                     gof = c(fitmeasures(fit_m.merit.nmerit)[[c(3)]],
                                             fitmeasures(fit_m.merit.nmerit)[[c(4)]],
                                             fitmeasures(fit_m.merit.nmerit)[[c(18)]],
                                             fitmeasures(fit_m.merit.nmerit)[[c(36)]],
                                             nobs(fit_m.merit.nmerit)),
                                     gof.names = ajuste,
                                     gof.decimal =c (TRUE, FALSE,TRUE, TRUE, FALSE))
m.ext.merit.nmerit <- texreg::createTexreg(coef.names  = ext.merit.nmerit$rhs ,
                                           coef = ext.merit.nmerit$est,
                                           se = ext.merit.nmerit$z,
                                           pvalues = ext.merit.nmerit$pvalue,
                                           gof = c(fitmeasures(fit_m.merit.nmerit)[[c(3)]],
                                                   fitmeasures(fit_m.merit.nmerit)[[c(4)]],
                                                   fitmeasures(fit_m.merit.nmerit)[[c(18)]],
                                                   fitmeasures(fit_m.merit.nmerit)[[c(36)]],
                                                   nobs(fit_m.merit.nmerit)),
                                           gof.names = ajuste,
                                           gof.decimal =c (TRUE, FALSE,TRUE, TRUE, FALSE))


############################ Modelo 7 (todas las variables) #############################


m.total <- '
# measurement model
## Endogenous factors 
Atribuciones_Internas=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
Atribuciones_Externas=~atrib_pob_4+atrib_pob_5+atrib_riq_4

## Exogenous factors
perc_merit=~merit_perc_effort+merit_perc_talent
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart

# Cov
atrib_riq_1 ~~ atrib_riq_3

# regressions

Atribuciones_Internas ~ sexo + edad + educ_tecnica + educ_uni_incomp + educ_uni_post + 
ingresos_1 + ingresos_2 + ingresos_3 + ingresos_4 +
act_prin_no_remunerado + act_prin_desempleado + act_prin_trabaja_estudia + act_prin_estudia + act_prin_jubliado
+ ess_ind + 
perc_merit + merit_pref_talent + merit_pref_effort + perc_nmerit + pref_nmerit
Atribuciones_Externas ~ sexo + edad + educ_tecnica + educ_uni_incomp + educ_uni_post + 
ingresos_1 + ingresos_2 + ingresos_3 + ingresos_4 +
act_prin_no_remunerado + act_prin_desempleado + act_prin_trabaja_estudia + act_prin_estudia + act_prin_jubliado
+ ess_ind + 
perc_merit + merit_pref_talent + merit_pref_effort + perc_nmerit + pref_nmerit
'

fit_total <- sem(m.total, data=data,ordered = T)
#summary(fit_total,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semoutput::sem_paths(fit_total, standardized = TRUE, ci = "standardized")
x <-  parameterEstimates(fit_total) #Para saber la posición de las filas
x
int.total <- as.data.frame(parameterEstimates(fit_total)[17:32,  ])
ext.total <- as.data.frame(parameterEstimates(fit_total)[33:48,  ])
fit.messem.inc <- fitMeasures(fit_total,c("chisq","df","pvalue","cfi","rmsea"))
#----Crear la tabla-----
m.int.total <- texreg::createTexreg(coef.names  = int.total$rhs ,
                                           coef = int.total$est,
                                           se = int.total$z,
                                           pvalues = int.total$pvalue,
                                           gof = c(fitmeasures(fit_total)[[c(3)]],
                                                   fitmeasures(fit_total)[[c(4)]],
                                                   fitmeasures(fit_total)[[c(18)]],
                                                   fitmeasures(fit_total)[[c(36)]],
                                                   nobs(fit_total)),
                                           gof.names = ajuste,
                                           gof.decimal =c (TRUE, FALSE,TRUE, TRUE, FALSE))
m.ext.total <- texreg::createTexreg(coef.names  = ext.total$rhs ,
                                    coef = ext.total$est,
                                    se = ext.total$z,
                                    pvalues = ext.total$pvalue,
                                    gof = c(fitmeasures(fit_total)[[c(3)]],
                                            fitmeasures(fit_total)[[c(4)]],
                                            fitmeasures(fit_total)[[c(18)]],
                                            fitmeasures(fit_total)[[c(36)]],
                                            nobs(fit_total)),
                                    gof.names = ajuste,
                                    gof.decimal =c (TRUE, FALSE,TRUE, TRUE, FALSE))


# ---- Tabla de todos los modelos

modelos.int1 <- list(m.int.obj,m.int.subj,m.int.obj.subj,m.int.merit,m.int.nmerit,m.int.merit.nmerit,m.int.total)
coefm.int1 <- c("Sexo",
            "Edad",
            "Educación técnica",
            "Educación universitaria incompleta",
            "Educación universitaria y postgrado",
            "De $358.001 a $448.000",
            "De $448.001 a $1.000.000 mensuales",
            "De $1.000.001 a $2.000.000",
            "Más de $2.000.001",
            "No remunerada",
            "Desempleado",
            "Trabaja y estudia",
            "Estudia",
            "Jubliado",
            "Estatus Social Subjetivo",
            "Percepción meritocrática",
            "Preferencia meritocrática",
            "Percepción no meritocrática",
            "Preferencia no meritocrática")
refm.int1 <- list("\\textbf{Educación (ref: Media completa o menos)}" = 3:5 ,
              "\\textbf{Rango ingresos mensuales hogar (ref: $358.000 o menos )}"= 6:9,
              "\\textbf{Actividad principal (ref: Trabajo jornada completa o parcial)}"=10:14)
semINT <- texreg(modelos.int1, center = TRUE,
                 custom.coef.names = coefm.int1,
                 custom.model.names = c("Modelo 1","Modelo 2","Modelo 3","Modelo 4","Modelo 5","Modelo 6","Modelo 7"),
                 dcolumn = TRUE,
                 booktabs = TRUE,
                 use.packages = FALSE,
                 single.row = FALSE,digits = 3,
                 sideways = FALSE,
                 no.margin = FALSE,scalebox = 0.65,
                 caption.above = TRUE,
#                 custom.note = nota3,
                 float.pos = "H",
                 groups = refm.int1,
                 label = "table:semREC",
#                 omit.coef = "(IDNR)|(isco_nr)|(ppol_cen)|(ppol_der)|(ppol_ind)|(ppol_ninguno)|(sexo)|(edad)",
                 caption = "Modelos de ecuaciones estructurales para variables independientes con la variable latente \\textit{Atribuciones internas} (1)")
semINT









m.fatal <- '
# measurement model
## Endogenous factors 
Atribuciones_Internas=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
Atribuciones_Externas=~atrib_pob_4+atrib_pob_5+atrib_riq_4
Atribuciones_Fatalistas=~atrib_pob_2+atrib_riq_2

## Exogenous factors
perc_merit=~merit_perc_effort+merit_perc_talent
pref_merit=~merit_pref_talent+merit_pref_effort
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart

# Cov
atrib_riq_1 ~~ atrib_riq_3

# regressions

Atribuciones_Internas ~ sexo + edad + educ_tecnica + educ_uni_incomp + educ_uni_post + 
ingresos_1 + ingresos_2 + ingresos_3 + ingresos_4 +
act_prin_no_remunerado + act_prin_desempleado + act_prin_trabaja_estudia + act_prin_estudia + act_prin_jubliado
+ ess_ind + 
perc_merit + pref_merit + perc_nmerit + pref_nmerit
Atribuciones_Externas ~ sexo + edad + educ_tecnica + educ_uni_incomp + educ_uni_post + 
ingresos_1 + ingresos_2 + ingresos_3 + ingresos_4 +
act_prin_no_remunerado + act_prin_desempleado + act_prin_trabaja_estudia + act_prin_estudia + act_prin_jubliado
+ ess_ind + 
perc_merit + pref_merit + perc_nmerit + pref_nmerit
Atribuciones_Fatalistas ~ sexo + edad + educ_tecnica + educ_uni_incomp + educ_uni_post + 
ingresos_1 + ingresos_2 + ingresos_3 + ingresos_4 +
act_prin_no_remunerado + act_prin_desempleado + act_prin_trabaja_estudia + act_prin_estudia + act_prin_jubliado
+ ess_ind + 
perc_merit + pref_merit + perc_nmerit + pref_nmerit
'

fit.fatal <- sem(m.fatal, data=data,ordered = T)
#summary(fit.fatal,fit.measures=TRUE,standardized=TRUE,rsquare=T)
semoutput::sem_paths(fit.fatal, standardized = TRUE, ci = "standardized")










# Stargazer table

pars.regressions.control <- standardizedSolution(reg.control.fit)[ standardizedSolution(reg.indeps.fit)[,'op']=='~', c(3,4,5,7)]
pars.regressions.indep <- standardizedSolution(reg.indeps.fit)[ standardizedSolution(reg.indeps.fit)[,'op']=='~', c(3,4,5,7)]

stargazer(pars.regressions.control, pars.regressions.indep, summary=FALSE, type='text', rownames=FALSE, initial.zero=FALSE, digits=3, title='Predicting Y')

#star.1 <- stargazer(pars.regressions.control, pars.regressions.indep, summary=FALSE, type='html', rownames=FALSE, initial.zero=FALSE, digits=3, title='Predicting Y')
#kable(star.1)


labels1 <- get_label(data)
lavaanPlot(model = reg.indeps.fit,labels = labels1, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"),coefs = TRUE, covs = F, stars = "regress")

save(reg.control.fit, file = "Output/Modelos/regmodel1.rda")
save(reg.merit.fit, file = "Output/Modelos/regmodel2.rda")
save(reg.indeps.fit, file = "Output/Modelos/regmodel3.rda")









# Modelo de mediación

## Primero checkear regresión sobre meritocracia

reg.med <- '
## Endogenous factors
perc_merit=~merit_perc_effort+merit_perc_talent
pref_merit=~merit_pref_talent+merit_pref_effort
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart

# regressions

perc_merit ~ educ_tecnica + educ_uni_incomp + educ_uni_post + sexo + ingresos + ess_ind + 
act_prin_no_remunerado + act_prin_desempleado + act_prin_trabaja_estudia + act_prin_jubliado
'

reg.med.fit <- sem(reg.med, data=data,ordered = T)

semoutput::sem_paths(reg.med.fit, standardized = TRUE)








med1 <- "
# Endogenous factor variable
Atribuciones_Internas=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
Atribuciones_Externas=~atrib_pob_4+atrib_pob_5+atrib_riq_4

# Exogenous factor variable
perc_merit=~merit_perc_effort+merit_perc_talent
pref_merit=~merit_pref_talent+merit_pref_effort
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart

# Cov
atrib_riq_1 ~~ atrib_riq_3

         # a path
         ess_ind ~ a*perc_merit

         # b path
         Atribuciones_Internas ~ b*ess_ind

         # c prime path 
         Atribuciones_Internas ~ cp*perc_merit

         # indirect and total effects
         ab := a*b
         total := cp + ab
"
f.med1 <- sem(med1, data = data, se = "bootstrap", bootstrap = 500)


summary(f.med1, standardized = TRUE)


med_per_to_ess <- c(0, "'.25**'", 0,
          0, 0, 0, 
          "'.04***'", "'.56*** (.06)'", 0)
matrix_per_to_ess<- matrix (nrow=3, ncol=3, byrow = TRUE, data=med_per_to_ess)
med_plot1<- plotmat (matrix_per_to_ess, pos=c(1,2), 
                name= c( "Estatus Social Subjetivo","Percepción meritocrática", "Atribuciones internas"), 
                box.type = "rect", box.size = 0.12, box.prop=0.5,  curve=0)

mediate(Atribuciones_Internas ~ ess_ind + perc_merit, data = f.med1, n.iter = 1000) %>% print(short = FALSE)








med2 <- "
# Endogenous factor variable
Atribuciones_Internas=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
Atribuciones_Externas=~atrib_pob_4+atrib_pob_5+atrib_riq_4

# Exogenous factor variable
perc_merit=~merit_perc_effort+merit_perc_talent
pref_merit=~merit_pref_talent+merit_pref_effort
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart

# Cov
atrib_riq_1 ~~ atrib_riq_3

         # a path
         perc_merit ~ a*ess_ind

         # b path
         Atribuciones_Internas ~ b*perc_merit

         # c prime path 
         Atribuciones_Internas ~ cp*ess_ind

         # indirect and total effects
         ab := a*b
         total := cp + ab
"
f.med2 <- sem(med2, data = data, se = "bootstrap", bootstrap = 500)


summary(f.med2, standardized = TRUE)


med_ess_to_per <- c(0, "'.04'", 0,
                    0, 0, 0, 
                    "'.44***'", "'.06*** (.01)'", 0)
matrix_ess_to_per<- matrix (nrow=3, ncol=3, byrow = TRUE, data=med_ess_to_per)
med_plot2 <- plotmat (matrix_ess_to_per, pos=c(1,2), 
                     name= c( "Percepción meritocrática","Estatus Social Subjetivo", "Atribuciones internas"), 
                     box.type = "rect", box.size = 0.12, box.prop=0.5,  curve=0)

med_plot2

plot(med_plot2)

med_plot2







med3 <- "
# Endogenous factor variable
Atribuciones_Internas=~atrib_pob_1+atrib_pob_3+atrib_riq_1+atrib_riq_3
Atribuciones_Externas=~atrib_pob_4+atrib_pob_5+atrib_riq_4

# Exogenous factor variable
perc_merit=~merit_perc_effort+merit_perc_talent
pref_merit=~merit_pref_talent+merit_pref_effort
perc_nmerit=~merit_perc_wpart+merit_perc_netw
pref_nmerit=~merit_pref_netw+merit_pref_wpart

# Cov
atrib_riq_1 ~~ atrib_riq_3

         # a path
         perc_merit ~ a*act_prin_desempleado
         perc_merit ~ a2*act_prin_trabaja_estudia
         perc_merit ~ a3*act_prin_estudia
         perc_merit ~ a4*act_prin_jubliado
         perc_merit ~ a5*act_prin_parcial
         perc_merit ~ a6*act_prin_completa

         # b path
         Atribuciones_Internas ~ b*perc_merit

         # c prime path 
         Atribuciones_Internas ~ cp*act_prin_desempleado

         # indirect and total effects
         ab := a*b
         total := cp + ab
"
f.med3 <- sem(med3, data = data, se = "bootstrap", bootstrap = 500)










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