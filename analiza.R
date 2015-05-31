source("functions/observation_split.R")
source("functions/count_instances.R")

library(dplyr)
library(lubridate)


######### WCZYTYWANIE DANYCH ------

## Wczytujemy tabelę danych z Jabłowa.

jablow <- read.csv2("input/jablow_calosc.csv", as.is = TRUE)

## Zmieniam daty na format R - patrz lubridate

jablow$Date_time <- dmy_hms(paste(jablow$Data, jablow$Time))

## Rozbijamy obserwacje z wieloma nietoperzami na pojedyncze obserwacje
jablow_pociete <- observation_split(jablow)
rm(jablow)

## Usuwamy dane zawierające oberwacje niepełne (u) - to jeszcze eksploracja

## Jeszcze tej częsci nie ma


## Liczymy zdarzenia w obrębie każdego kodu

characters <-c("a","l","t","r","p","c","d","f","n","e")

zdarzenia <- count_instances(jablow_pociete, characters)


# Łączę tabele ze zdarzeniami i tabelę wejściową
jablow_zdarzenia <- cbind(jablow_pociete, zdarzenia)

# Zamieniam na wygodniejszy format - patrz dplyr
jablow_zdarzenia <- tbl_df(jablow_zdarzenia)



## Takie se powiedzmy eksploracje ----


# Grupuję po sezonach 
jablow_zdarzenia<- group_by(jablow_zdarzenia, Season)

# Liczę ilość pościgów i ilość zdarzeń, a następniew wyliczam średnią ilośc
# pościgów na sezon
summary <- summarise(jablow_zdarzenia, sum_p =sum(p, na.rm = TRUE), n = length(p))
summary <- mutate(summary, mean_p = sum_p/n)    
