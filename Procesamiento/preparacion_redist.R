# El presente código es un avance hacia una posible extensión de la investigación sobre "Efectos de las creencias
# meritocráticas sobre las atribuciones de pobreza y riqueza", en donde se analice la relación de variables 
# meritocráticas, atribuciones de pobreza y riqueza y políticas redistributivas.




pacman::p_load(dplyr, sjmisc, car, sjlabelled,labelled, stargazer,kableExtra,corrplot,sessioninfo,readxl,
               pander,xtable, tidyverse, extrafont, ggplot2, forcats, ggpubr, naniar,haven, devtools,summarytools,
               webshot,poLCA,sjPlot,lavaan)

data_ola1 <- read_spss("Input/factorial_ola1.sav")
data_original <- read_spss("Input/factorial_ola2.sav")

data_ola1$IPAddress[data_ola1$IPAddress %in% data_original$IPAddress] # Chequear cuantos casos se comparten en ambas olas

data_redist<- data_ola1 %>% dplyr::select(IPAddress,redist)

data_redist<-merge(data_original, data_redist, by.x='IPAddress', by.y='IPAddress') # this works!

find_var(data_original, "ess")
frq(data_original$atrib_riq_5)

data_redist <- data_redist %>% dplyr::select(atrib_pob_1,atrib_pob_3, # Atribuciones Internas pobreza
                                             atrib_pob_4,atrib_pob_5, # Atribuciones Externas pobreza
                                        atrib_riq_1,atrib_riq_3, # Atribuciones Internas riqueza
                                        atrib_riq_4,atrib_riq_5, # Atribuciones Internas riqueza
                                        merit_perc_effort,merit_perc_talent,merit_perc_wpart,merit_perc_netw, # percepciones merit.
                                        merit_pref_effort,merit_pref_talent,merit_pref_wpart,merit_pref_netw, # preferencias merit.
                                        sexo, # Sexo
                                        edad, # Edad
                                        educ, # Escolarización
                                        ingresos, # Tramos ingresos
                                        ess_ind, # Estatus social subjetivo
                                        consen, # Variable consentimiento (para ver N valido)
                               redist) # Redistribucion estatal


# N valido

data_redist<-subset(data_redist, consen!=2)
dim(data_redist)
data_redist = subset(data_redist, select = -consen )


# Descriptivos

descriptivos_redist<-dfSummary(data_redist, headings=FALSE, valid.col = FALSE, na.col = FALSE, varnumbers = FALSE)
view(descriptivos_redist)
print(descriptivos_redist, method = "pander", file = "Output/descriptivos_redist.html")

dim(data_redist)
sum(is.na(data_redist))
data_redist <-na.omit(data_redist)
dim(data_redist)



# Etiquetas para modelos de regresion

data_redist$atrib_pob_1 <- set_label(x = data_redist$atrib_pob_1,label = "Atribuciones pobreza Falta habilidad")
data_redist$atrib_pob_3 <- set_label(x = data_redist$atrib_pob_3,label = "Atribuciones pobreza Falta esfuerzo")
data_redist$atrib_pob_4 <- set_label(x = data_redist$atrib_pob_4,label = "Atribuciones pobreza Sistema económico")
data_redist$atrib_pob_5 <- set_label(x = data_redist$atrib_pob_5,label = "Atribuciones pobreza Sistema educativo")
data_redist$atrib_riq_1 <- set_label(x = data_redist$atrib_riq_1,label = "Atribuciones riqueza Talento")
data_redist$atrib_riq_3 <- set_label(x = data_redist$atrib_riq_3,label = "Atribuciones riqueza Trabajo duro")
data_redist$atrib_riq_4 <- set_label(x = data_redist$atrib_riq_4,label = "Atribuciones riqueza Sistema económico")
data_redist$atrib_riq_5 <- set_label(x = data_redist$atrib_riq_5,label = "Atribuciones riqueza Sistema educativo")
data_redist$merit_perc_effort <- set_label(x = data_redist$merit_perc_effort,label = "Percepcion meritocracia: Esfuerzo")
data_redist$merit_perc_talent <- set_label(x = data_redist$merit_perc_talent,label = "Percepcion meritocracia: Talento")
data_redist$merit_pref_effort <- set_label(x = data_redist$merit_pref_effort,label = "Preferencia meritocracia: Esfuerzo")
data_redist$merit_pref_talent <- set_label(x = data_redist$merit_pref_talent,label = "Preferencia meritocracia: Talento")
data_redist$sexo <- set_label(x = data_redist$sexo,label = "Sexo (Ref=hombre)")
data_redist$edad <- set_label(x = data_redist$edad,label = "Edad")
data_redist$sexo <- set_label(x = data_redist$sexo,label = "Sexo (Ref=hombre)")
data_redist$educ <- set_label(x = data_redist$educ,label = "Nivel escolarización")
data_redist$ingresos <- set_label(x = data_redist$ingresos,label = "Tramo ingresos")
data_redist$ess_ind <- set_label(x = data_redist$ess_ind,label = "Estatus social subjetivo")
data_redist$redist <- set_label(x = data_redist$redist,label = "Redistribución estatal")


# Guardar base de datos

save(data_redist,file = "Input/Data_proc/data_redist.RData")
