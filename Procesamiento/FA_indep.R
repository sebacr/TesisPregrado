pacman::p_load(dplyr, car, sjlabelled,labelled, stargazer,kableExtra,corrplot,
               tidyverse, ggplot2, ggpubr,haven, devtools,summarytools,nortest,tseries,psych,
               sjPlot,lavaan,semPlot,pander,standarize)
#install.packages("webshot",dependencies = TRUE)
library(webshot)
webshot::install_phantomjs(force = TRUE)
load("Input/Data_proc/data.RData") # Data percepcion 2019

data %>% 
  dplyr::select(merit_perc_effort,merit_perc_talent,merit_perc_wpart) %>% 
  cor(use = "complete.obs") -> meritcor

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