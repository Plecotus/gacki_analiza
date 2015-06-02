## Importowanie funkcji potrzebnych do analizy

source("functions/observation_split.R")
source("functions/count_instances.R")
source("functions/u_remove.R")


library(dplyr)
library(lubridate)


######### WCZYTYWANIE DANYCH ------

## Wczytujemy tabelę danych z Jabłowa.

jablow <- read.csv2("input/jablow_calosc.csv", as.is = TRUE)

## Zmieniam daty na format R - patrz lubridate

jablow$Date_time <- dmy_hms(paste(jablow$Data, jablow$Time))

## Usuwamy dane zawierające oberwacje niepełne (u), bez podmianki wewnętrzych
# elementów na "/", kontroluje parametr slash

jablow <- u_remove(jablow, slash = F)

## Rozbijamy obserwacje z wieloma nietoperzami na pojedyncze obserwacje
jablow_pociete <- observation_split(jablow)
rm(jablow)



## Liczymy zdarzenia w obrębie każdego kodu

characters <-c("a","l","t","r","p","c","d","f","n","e", "u")

zdarzenia <- count_instances(jablow_pociete, characters)


# Łączę tabele ze zdarzeniami i tabelę wejściową
jablow_zdarzenia <- cbind(jablow_pociete, zdarzenia)

# Zamieniam na wygodniejszy format - patrz dplyr
jablow_zdarzenia <- tbl_df(jablow_zdarzenia)


## Zapisuje dane do tabeli zewnętrznej

write.csv(jablow_zdarzenia, "output/jablow_zdarzenia")


## Takie se powiedzmy eksploracje ----


# Grupuję po sezonach 
jablow_zdarzenia <- group_by(jablow_zdarzenia, Season)

# Liczę ilość pościgów i ilość zdarzeń, a następniew wyliczam średnią ilośc
# pościgów na sezon
summary_jablow <- summarise(jablow_zdarzenia, sum_p =sum(p, na.rm = TRUE), n = length(p))
summary_jablow<- mutate(summary_jablow, mean_p = sum_p/n)    
