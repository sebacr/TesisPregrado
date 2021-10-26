pacman::p_load(dplyr, sjmisc, car, sjlabelled,labelled, stargazer,kableExtra,corrplot,sessioninfo,readxl,
               pander,xtable, tidyverse, extrafont, ggplot2, forcats, ggpubr, naniar,haven, devtools,summarytools,
               webshot,poLCA,sjPlot,lavaan)

load("Input/factorial_ola2.RData") 

find_var(factorial_ola2, "ess")
frq(factorial_ola2$ess_ind)



data <- factorial_ola2 %>% 
  dplyr::select(atrib_pob_1,atrib_pob_2,atrib_pob_3,atrib_pob_4,atrib_pob_5, # Atribuciones pobreza
                atrib_riq_1,atrib_riq_2,atrib_riq_3,atrib_riq_4,atrib_riq_5, # Atribuciones riqueza
                merit_perc_effort,merit_perc_talent,merit_perc_wpart,merit_perc_netw, # percepciones merit.
                merit_pref_effort,merit_pref_talent,merit_pref_wpart,merit_pref_netw, # preferencias merit.
                sexo, # Sexo
                edad, # Edad
                educ, # Escolarización
                ingresos, # Tramos ingresos
                ess_ind, # Estatus social subjetivo
                consen) # Variable consentimiento (para ver N valido)

# N valido

dim(data)
data<-subset(data, consen!=2)
dim(data)
data = subset(data, select = -consen )


dim(data)
sum(is.na(data))
data <-na.omit(data)
dim(data)

view(dfSummary(data, headings=FALSE, valid.col = FALSE, na.col = FALSE, varnumbers = FALSE))

# recodificacion educ 
data$educ <- car::recode(data$educ, "c(1,2,3)=1; c(4,5)=2;c(6,7)=3;c(8,9,10)=4")
data$educ <- set_labels(data$educ,
                             labels=c( "Primaria incompleta o completa"=1,
                                       "Secundaria incompleta o completa"=2,
                                       "Superior no universitaria completa o incompleta"=3,
                                       "Universitaria o postgrados completa o incompleta"=4))


# recodificacion ingresos
frq(data$ess_ind)
data$ingresos <- car::recode(data$ingresos, "c(1,2,3,4,5,6)=1; c(7,8,9)=2;10=3;11=4;12=5;13=6;14=7")
data$ingresos <- set_labels(data$ingresos,
                        labels=c( "Menos de $179.000"=1,
                                  "De $179.001 a $358.000"=2,
                                  "De $358.001 a $448.000"=3,
                                  "De $448.001 a $1.000.000"=4,
                                  "De $1.000.001 a $2.000.000"=5,
                                  "De $2.000.001 a $3.000.000"=6,
                                  "Más de $3.000.000"=7))

# Etiquetas para modelos de regresion

data$atrib_pob_1 <- set_label(x = data$atrib_pob_1,label = "Atribuciones pobreza Falta habilidad")
data$atrib_pob_2 <- set_label(x = data$atrib_pob_2,label = "Atribuciones pobreza Mala suerte")
data$atrib_pob_3 <- set_label(x = data$atrib_pob_3,label = "Atribuciones pobreza Falta esfuerzo")
data$atrib_pob_4 <- set_label(x = data$atrib_pob_4,label = "Atribuciones pobreza Sistema económico")
data$atrib_pob_5 <- set_label(x = data$atrib_pob_5,label = "Atribuciones pobreza Sistema educativo")
data$atrib_riq_1 <- set_label(x = data$atrib_riq_1,label = "Atribuciones riqueza Talento")
data$atrib_riq_2 <- set_label(x = data$atrib_riq_2,label = "Atribuciones riqueza Suerte")
data$atrib_riq_3 <- set_label(x = data$atrib_riq_3,label = "Atribuciones riqueza Trabajo duro")
data$atrib_riq_4 <- set_label(x = data$atrib_riq_4,label = "Atribuciones riqueza Sistema económico")
data$atrib_riq_5 <- set_label(x = data$atrib_riq_5,label = "Atribuciones riqueza Sistema educativo")
data$merit_perc_effort <- set_label(x = data$merit_perc_effort,label = "Obtienen mayores recompensas: Esfuerzo")
data$merit_perc_talent <- set_label(x = data$merit_perc_talent,label = "Obtienen mayores recompensas: Talento")
data$merit_perc_wpart <- set_label(x = data$merit_perc_wpart,label = "Logran salir adelante: Padres ricos")
data$merit_perc_netw <- set_label(x = data$merit_perc_netw,label = "Logran salir adelante: Buenos contactos")
data$merit_pref_effort <- set_label(x = data$merit_pref_effort,label = "Deberían obtener mayores recompensas: Esfuerzo")
data$merit_pref_talent <- set_label(x = data$merit_pref_talent,label = "Deberían obtener mayores recompensas: Talento")
data$merit_pref_wpart <- set_label(x = data$merit_pref_wpart,label = "Esta bien que salgan adelante: Padres ricos")
data$merit_pref_netw <- set_label(x = data$merit_pref_netw,label = "Esta bien que salgan adelante: Buenos contactos")
data$sexo <- set_label(x = data$sexo,label = "Sexo (Ref=hombre)")
data$edad <- set_label(x = data$edad,label = "Edad")
data$sexo <- set_label(x = data$sexo,label = "Sexo (Ref=hombre)")
data$educ <- set_label(x = data$educ,label = "Nivel escolarización")
data$ingresos <- set_label(x = data$ingresos,label = "Tramo ingresos")
data$ess_ind <- set_label(x = data$ess_ind,label = "Estatus social subjetivo")

# Descriptivos

df_descriptivos<-dfSummary(data, headings=FALSE, valid.col = FALSE, na.col = FALSE, varnumbers = FALSE)
view(df_descriptivos)
print(df_descriptivos, method = "pander", file = "Output/df_descriptivos.html")

# Guardar base de datos

save(data,file = "Input/Data_proc/data.RData")
