## Importowanie funkcji potrzebnych do analizy

source("functions/observation_split.R")
source("functions/count_instances.R")
source("functions/u_remove.R")


library(dplyr)
library(lubridate)


######### WCZYTYWANIE DANYCH ------

## Wczytujemy tabelę danych z Jabłowa.

krajanow <- read.csv2("input/krajanow_calosc.csv", as.is = TRUE)

## Zmieniam daty na format R - patrz lubridate

krajanow$Date_time <- dmy_hms(paste(krajanow$Data, krajanow$Time))


# Ustalamy porę dnia (wieczór/rano)
krajanow$godzina <- hour(krajanow$Date_time)
krajanow$pora_dnia <- ifelse(krajanow$godzina>=12, "wieczor", "rano")


## Usuwamy dane zawierające oberwacje niepełne (u) - to jeszcze eksploracja

krajanow <- u_remove(krajanow, slash = F)

## Rozbijamy obserwacje z wieloma nietoperzami na pojedyncze obserwacje
krajanow_pociete <- observation_split(krajanow)
rm(krajanow)



## Liczymy zdarzenia w obrębie każdego kodu

characters <-c("a","l","t","r","p","c","d","f","n","e", "u")

zdarzenia <- count_instances(krajanow_pociete, characters)


# Łączę tabele ze zdarzeniami i tabelę wejściową
krajanow_zdarzenia <- cbind(krajanow_pociete, zdarzenia)

# Zamieniam na wygodniejszy format - patrz dplyr
krajanow_zdarzenia <- tbl_df(krajanow_zdarzenia)


## Zapisuje dane do tabeli zewnętrznej

write.csv(krajanow_zdarzenia, "output/krajanow_zdarzenia.csv")


## Takie se powiedzmy eksploracje ----

# Grupuję po sezonach 
krajanow_zdarzenia <- group_by(krajanow_zdarzenia, Season, Night, pora_dnia)

# Liczę ilość pościgów i ilość zdarzeń, a następniew wyliczam średnią ilośc
# pościgów na sezon

summary_krajanow <- summarise(krajanow_zdarzenia,
                            sum_p =sum(as.numeric(as.character(p)), na.rm = TRUE),
                            n = length(p))

summary_krajanow <- mutate(summary_krajanow, mean_p = sum_p/n)    

boxplot(mean_p ~ pora_dnia, data = summary_krajanow)
g <- ggplot(summary_krajanow, aes(x = pora_dnia, y = mean_p))
g + geom_boxplot() + facet_grid(. ~ Season)
