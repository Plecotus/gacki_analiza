## Importowanie funkcji potrzebnych do analizy

source("functions/observation_split.R")
source("functions/count_instances.R")
source("functions/u_remove.R")

library(dplyr)
library(lubridate)
library(ggplot2)
library(caret)


######### WCZYTYWANIE DANYCH ------
## Wczytujemy tabelę danych z Jabłowa.

jablow <- read.csv2("input/jablow_calosc.csv", as.is = TRUE)

## Usuwamy niepotrzebne kolumny, ktore załadowały się z pliku 'csv', 
# czyli kolumny od ósmej w górę

jablow <- jablow[, 1:8]

## Zmieniam daty na format R - patrz lubridate
jablow$Date_time <- dmy_hms(paste(jablow$Data, jablow$Time))

# Ustalamy porę dnia (wieczór/rano)
jablow$godzina <- hour(jablow$Date_time)
jablow$pora_dnia <- ifelse(jablow$godzina>=12, "wieczor", "rano")


## Usuwamy dane zawierające oberwacje niepełne (u), bez podmianki wewnętrzych
# elementów na "/", kontroluje parametr slash

jablow <- u_remove(jablow, slash = F)

## Rozbijamy obserwacje z wieloma nietoperzami na pojedyncze obserwacje
jablow_pociete <- observation_split(jablow)
rm(jablow)



## Liczymy zdarzenia w obrębie każdego kodu

characters <-c("a","l","t","r","p","c","d","f","n","e", "u", "g", "h", "i", "k","o", "w", "v")
zdarzenia <- count_instances(jablow_pociete, characters)



# Łączę tabele ze zdarzeniami i tabelę wejściową
jablow_zdarzenia <- cbind(jablow_pociete, zdarzenia)

# Zamieniam na wygodniejszy format - patrz dplyr
jablow_zdarzenia <- tbl_df(jablow_zdarzenia)


## Zapisuje dane do tabeli zewnętrznej

write.csv(jablow_zdarzenia, "output/jablow_zdarzenia.csv")



## Takie se powiedzmy eksploracje ----


## Sumujemy ilość elementów z zdarzeniu
jablow_zdarzenia$ilosc_elementow <- apply(jablow_zdarzenia[,12:29],1,sum, na.rm = T)


# Grupuję po sezonach 
jablow_zdarzenia <- group_by(jablow_zdarzenia, Season, Night,Toys, pora_dnia)

# Liczę ilość pościgów i ilość zdarzeń, a następniew wyliczam średnią ilośc
# pościgów na sezon

summary_jablow <- summarise_each(jablow_zdarzenia,
                                 funs(
                                    sum(., na.rm = T)
                                 ), vars = -c(1:11))
summary_jablow$n_observation <- summarise(jablow_zdarzenia, n_observ = n())$n_observ 
srednie_zlozonosci <- summarise(jablow_zdarzenia,
                                srednia_sekwencja = mean(ilosc_elementow))                         

summary_jablow <- mutate(summary_jablow, mean_p = p/n_observation)    


boxplot(mean_p ~ pora_dnia, data = summary_jablow)
g <- ggplot(summary_jablow, aes(x = pora_dnia, y = mean_p))
g + geom_boxplot() + facet_grid(. ~ Toys)+ geom_jitter()


## Drzewo klasyfikacyjne

to_fit <- summary_jablow[,c(1,5:22)]
fit <- train(Season ~ ., data = to_fit, method = "rpart")
summary(fit)

library(ca)
ca(Season ~ ., data = as.table(to_fit))
