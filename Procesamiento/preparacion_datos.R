pacman::p_load(dplyr, sjmisc, car, sjlabelled,labelled, stargazer,kableExtra,corrplot,sessioninfo,readxl,
               pander,xtable, tidyverse, extrafont, ggplot2, forcats, ggpubr, naniar,haven, devtools,summarytools,
               webshot,poLCA,sjPlot,lavaan,fastDummies,arsenal,generics,diffdf,plyr)

load("Input/factorial_ola2.RData") 

#load("Input/factorial_ola1.RData") 
#dfcombo <- match_df(newdf1, newdf2, on="LocationLongitude")
#target <- Reduce(intersect, list(colnames(factorial_ola1), colnames(factorial_ola2)))
#newdf1 <- factorial_ola1[target]
#newdf2 <- factorial_ola2[target]
#obj<-comparedf(newdf1,newdf2)
#print(obj$vars.summary)
#summary(comparedf(newdf1,newdf2, by = "IPAddress"))
#inner_join(factorial_ola1,factorial_ola2)
#duplicated(factorial_ola1$Duration__in_seconds_) # the rows of dat var1 duplicated
#diffdf(factorial_ola1,factorial_ola2)
#data_common1 <- generics::intersect(newdf1,newdf2)  # Apply intersect function
#data_common1                                       # Print common data
#data_common2 <- dplyr::inner_join(newdf1,newdf2)           # Apply inner_join function
#data_common2   
#id1 <- factorial_ola1 %>% 
#  dplyr::select(ResponseId,IPAddress,RecipientLastName,RecipientEmail,RecipientFirstName,Duration__in_seconds_,
#                LocationLongitude,LocationLatitude,edad)
#id2 <- factorial_ola2 %>% 
#  dplyr::select(ResponseId,IPAddress,RecipientLastName,RecipientEmail,RecipientFirstName,Duration__in_seconds_,
#                LocationLongitude,LocationLatitude,edad)
#checkID <- rbind(id1,id2)
#factorial_ola1$IPAddress[duplicated(factorial_ola1$Duration__in_seconds_)]
#factorial_ola1<-factorial_ola1[!duplicated(factorial_ola1$Duration__in_seconds_), ]

#options(max.print=1000000000)
#colnames(factorial_ola2)
#factorial_ola1$ResponseId


find_var(factorial_ola2, "salir")
#frq(factorial_ola2$ess_ind)



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
                act_prin, # actividad principal
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
frq(data$educ)
data$educ<-remove_val_labels(data$educ)
data$educ <- car::recode(data$educ, "1:5='med_comp';6:7='tecnica';8='uni_incomp';9:10='uni_post'")
data <- dummy_cols(data,select_columns = "educ")
data = subset(data, select = -educ) # Eliminamos variable educ

#data$educ_med_incomp <- set_label(x = data$educ_med_incomp,label = "Secundaria incompleta o menos")
data$educ_med_comp <- set_label(x = data$educ_med_comp,label = "Secundaria completa o menos")
data$educ_tecnica <- set_label(x = data$educ_tecnica,label = "Terciaria no universitaria incompleta o completa")
data$educ_uni_incomp <- set_label(x = data$educ_uni_incomp,label = "Terciaria universitaria incompleta")
data$educ_uni_post <- set_label(x = data$educ_uni_post,label = "Terciaria y Postgrado")


#data$educ <- car::recode(data$educ, "1:3=1;4=2;5=3;6=4;7=5;8=6;9=7;10=8")
#data$educ <- as.numeric(data$educ)
#data$educ <- set_labels(data$educ,
#                        labels=c("Educación básica completa o menos"=1, 
#                          "Educación media incompleta"=2,
#                                  "Educación media completa"=3,
#                                  "Educación superior no universitaria incompleta"=4,
#                                  "Educación superior no universitaria completa"=5,
#                          "Educación universitaria incompleta"=6,
#                          "Educación universitaria completa"=7,
#                          "Estudios de postgrado, master, doctorado"=8))


# recodificacion ingresos
frq(data$ingresos)
data$ingresos <- car::recode(data$ingresos, "1:9=0;10=1;11=2;12=3;13:14=4")
data$ingresos <- as.character(data$ingresos)
data <- dummy_cols(data,select_columns = "ingresos")
data = subset(data, select = -ingresos) # Eliminamos variable educ
data$ingresos_0 <- set_label(x = data$ingresos_0,label = "Menos de $56.000")
data$ingresos_1 <- set_label(x = data$ingresos_1,label = "De $56.001 a $101.000")
data$ingresos_2 <- set_label(x = data$ingresos_2,label = "De $101.001 a $179.000")
data$ingresos_3 <- set_label(x = data$ingresos_3,label = "De $179.001 a $291.000")
data$ingresos_3 <- set_label(x = data$ingresos_3,label = "De $291.001 a $358.000")
data$ingresos_3 <- set_label(x = data$ingresos_3,label = "De $358.001 a $448.000")
data$ingresos_3 <- set_label(x = data$ingresos_3,label = "De $448.001 a $1.000.000")
data$ingresos_3 <- set_label(x = data$ingresos_3,label = "De $1.000.001 a $2.000.000")
data$ingresos_3 <- set_label(x = data$ingresos_3,label = "De $2.000.001 a $3.000.000")
data$ingresos_3 <- set_label(x = data$ingresos_3,label = "Más de $3.000.000")

#data$ingresos <- set_labels(data$ingresos,
#                        labels=c( "Menos de $56.000"=0,
#                                  "De $56.001 a $101.000"=1,
#                                  "De $101.001 a $179.000"=2,
#                                  "De $179.001 a $291.000"=3,
#                                  "De $291.001 a $358.000"=4,
#                                  "De $358.001 a $448.000"=5,
#                                  "De $448.001 a $1.000.000"=6,
#                                  "De $1.000.001 a $2.000.000"=7,
#                                  "De $2.000.001 a $3.000.000"=8,
#                                  "Más de $3.000.000"=9))

# Recodificación actividad principal

frq(data$act_prin)
data$act_prin<-remove_val_labels(data$act_prin)
#data$act_prin <- car::recode(data$act_prin, "1:2=1;else=0")

#data$act_prin <- car::recode(data$act_prin, "7:9=1;3:4=2;5:6=3;2=4;1=5")
#data$act_prin <- set_labels(data$act_prin,
#                            labels=c( "Sin actividad/no remunerada"=1,
#                                      "Trabaja y/o estudia"=2,
#                                      "Jubilado/desempleado"=3,
#                                      "Trabaja jornada parcial"=4,
#                                      "Trabaja jornada completa"=5))

data$act_prin <- car::recode(data$act_prin, "1:2='completa';3='trabaja_estudia';4='estudia';5='jubliado';
                             6='desempleado';7:9='no_remunerado'")
data <- dummy_cols(data,select_columns = "act_prin")
data = subset(data, select = -act_prin) # Eliminamos variable actividad principal

data$act_prin_completa <- set_label(x = data$act_prin_completa,label = "Jornada completa/parcial")
#data$act_prin_parcial<- set_label(x = data$act_prin_parcial,label = "Jornada parcial")
data$act_prin_desempleado <- set_label(x = data$act_prin_desempleado,label = "Desempleado")
data$act_prin_jubliado <- set_label(x = data$act_prin_jubliado,label = "Jubliado o pensionado")
data$act_prin_trabaja_estudia <- set_label(x = data$act_prin_trabaja_estudia,label = "Trabaja y/o estudia")
data$act_prin_no_remunerado <- set_label(x = data$act_prin_no_remunerado,
                                                      label = "Trabajo no remunerado, no realiza actividades o enfermo/discapacidad")



# recodificación ESS

frq(data$ess_ind)
data$ess_ind <- car::recode(data$ess_ind, "36=10;37=9;38=8;39=7;40=6;41=5;42=4;43=3;44=2;45=1;46=0")
data$ess_ind<-remove_val_labels(data$ess_ind)

# recodificacion sexo

frq(data$sexo)
data$sexo <- car::recode(data$sexo, "1=0;2=1")
data$sexo<- set_labels(data$sexo,labels=c( "Hombre"=0,
                                      "Mujer"=1))
frq(data$sexo)

# Etiquetas 

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
data$educ <- set_label(x = data$educ,label = "Nivel escolarización")
data$ingresos <- set_label(x = data$ingresos,label = "Tramo ingresos")
data$ess_ind <- set_label(x = data$ess_ind,label = "Estatus social subjetivo")

# Descriptivos
#load("Input/Data_proc/data.RData") 

df_descriptivos<-dfSummary(data, headings=FALSE, valid.col = FALSE, na.col = FALSE, varnumbers = FALSE)
view(df_descriptivos)
print(df_descriptivos, method = "pander", file = "Output/Tablas/df_descriptivos.html")

# Guardar base de datos

save(data,file = "Input/Data_proc/data.RData")
