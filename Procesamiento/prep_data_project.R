library(sjlabelled)
library(stringr)
library(stringi)
library(dplyr)
library(datasets)
library(data.table)
library(tidyr)
library(summarytools)
library(haven)
library(foreign)
library(readxl)

options(scipen=999)
rm(list=ls())

data01 <- read_spss("Input/factorial_mod-v01_June.sav")
#load(file = "input/data/proc/vig_recode.RData")
#vig_recode<-read_excel("Input/vig_recode.xlsx")

data01 <- data01 %>% filter(Finished==1,Progress==100,!is.na(Q5231))

# view(dfSummary(x = data01))


deck_cod01<- c("FL_431_DO_","FL_446_DO_","FL_467_DO_","FL_481_DO_","FL_494_DO_",
               "FL_507_DO_","FL_520_DO_","FL_533_DO_","FL_546_DO_","FL_559_DO_")

decks01<- data01 %>% select(ResponseId,starts_with("p_a_"),starts_with("p_b_"),matches(paste(deck_cod01, collapse="|")))

for (i in deck_cod01) {
  names(decks01) <- names(decks01) %>% str_replace_all(pattern = i,replacement = "vig") 
}

decks01<- remove_label(decks01) 
nam01<- decks01 %>% select(starts_with("vig")) %>% names()

#-no borar--#
# a1<- decks01[decks01 %>% select(starts_with("vig")) %>% names()]
# # a2 <- !(is.na(a1[nam01]))
# w <- which(!(is.na(a1[nam01]))=="TRUE",arr.ind=TRUE)
# a1[w] <- names(a1)[w[,"col"]]
#-----------------------------------#
decks02 <- data.frame(!(is.na(decks01[nam01])));decks02[decks02==FALSE] <- NA
decks03 <- bind_cols(select(decks01,-nam01),decks02)

w <- which(decks03=="TRUE",arr.ind=TRUE)
decks03[w] <- names(decks03)[w[,"col"]]




# wide a long -------------------------------------------------------------
wide <- setDT(decks03)

long01<- melt(wide, id.vars = "ResponseId",
              variable.name = "wave",
              measure = patterns("^p_a_","^p_b_","vig"),
              value.name = c("taxperc","taxjust","vig"));long01$wave <- NULL
long01 <- na.omit(long01)
long01$vig  <- str_replace_all(long01$vig,pattern = "vig",replacement = "")
long01$vig  <- as.numeric(long01$vig)


long01$taxperc <- stri_replace_all(long01$taxperc, "", fixed=c("$")) #delete $ symbol
long01$taxperc <- stri_replace_all(long01$taxperc, "", fixed=c(".")) #delete . 
long01$taxperc <- as.numeric(long01$taxperc) # transform to numeric
summary(long01$taxperc) 
                 
long01$taxjust <- stri_replace_all(long01$taxjust, "", fixed=c("$")) #delete $ symbol
long01$taxjust <- stri_replace_all(long01$taxjust, "", fixed=c(".")) #delete . 
long01$taxjust <- as.numeric(long01$taxjust) # transform to numeric
summary(long01$taxjust) 


# pegar a base long sociodemograficos de respondente ----------------------

long02<- left_join(long01,data01[,c("ResponseId","sexo","edad","educ")])      




