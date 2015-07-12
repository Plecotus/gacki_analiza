## Importowanie funkcji potrzebnych do analizy

source("functions/observation_split.R")
source("functions/count_instances.R")
source("functions/u_remove.R")


# Wczytujemy potrzebne pakiety
library(dplyr)
library(lubridate)
library(ggplot2)



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

## Usuwamy zdarzenia sprzed godziny 20

jablow <- filter(jablow, godzina>=20 | godzina<7)


## Usuwamy dane zawierające oberwacje niepełne (u), bez podmianki wewnętrzych
# elementów na "/", kontroluje parametr slash

jablow <- u_remove(jablow, slash = F)

## Rozbijamy obserwacje z wieloma nietoperzami na pojedyncze obserwacje
jablow_pociete <- observation_split(jablow)
rm(jablow)



## Liczymy zdarzenia w obrębie każdego kodu

characters <-c("a","l","t","r","p","c","d","f","n","e", "u", "g", "h", "i", "k","o", "w", "v")
zdarzenia <- count_instances(jablow_pociete, characters)



# Łączymy tabele ze zdarzeniami i tabelę wejściową

jablow_zdarzenia <- cbind(jablow_pociete, zdarzenia)
rm(jablow_pociete)
rm(zdarzenia)

# Zamieniamy na wygodniejszy format - patrz dplyr
jablow_zdarzenia <- tbl_df(jablow_zdarzenia)




## Sumujemy ilość elementów z zdarzeniu
jablow_zdarzenia$ilosc_elementow <- apply(jablow_zdarzenia[,12:29],1,sum, na.rm = T)


## Od ilości elementów odejmujemy wartość p, ponieważ uznaliśmy, że nie jest to
## konkretna ewolujca tylko oznacza interakcję pomiędzy nietoperzami
jablow_zdarzenia <- mutate(jablow_zdarzenia, 
                           ilosc_elementow = ifelse(is.na(p), ilosc_elementow,ilosc_elementow-p))



## USUWANIE EVENTÓW_V, które podminieniły sie na '', w  wyniku użycia funkcji u_remove

jablow_zdarzenia <- filter(jablow_zdarzenia, Event_V != '')



## Zapisuje dane do tabeli zewnętrznej

write.csv(jablow_zdarzenia, "output/jablow_zdarzenia.csv")



### ANALIZA POWIEDZMY, ŻE STATYSTYCZNA ------

# Grupujemy po sezonach, nocach i porze dnia, żeby potem policzyć sumy dla konkretnych
# zdarzeń
jablow_zdarzenia <- group_by(jablow_zdarzenia, Season, Channel, Night, pora_dnia)

### Liczymy liczbę pościgów i liczbę zdarzeń

summary_jablow <- summarise_each(jablow_zdarzenia,
                                 funs(
                                    sum(., na.rm = T)
                                 ), vars = -c(1:11))


# Liczymy średnią liczbę pościgów
# Ilość obserwacji
summary_jablow$n_observation <- summarise(jablow_zdarzenia, n_observ = n())$n_observ 

# Dodajemy średnie ilości pościgów 
summary_jablow <- mutate(summary_jablow, mean_p = p/n_observation)    

# Średnie złożoności, czyli średnia ilość elementów w pojedynczej obserwacji
summary_jablow$srednia_sekwencja <- summarise(jablow_zdarzenia,
                                              srednia_sekwencja = mean(ilosc_elementow))$srednia_sekwencja




#### WYKRESY -------

# Średnie ilości pościgów 
g <- ggplot(summary_jablow, aes(x = pora_dnia, y = mean_p))

# Tylko za podziałem na porę dnia
g + geom_boxplot()

# Z podziałem tylko na sezon

g + geom_boxplot(aes(x = Season)) + theme_bw() + geom_jitter(aes(x = Season)) 


# Z podziałem na sezon i porę dnia
g + geom_boxplot() + facet_grid(. ~ Season) + theme_bw() + geom_jitter()



# Złożoność sekwencji a sezon

s <- ggplot(jablow_zdarzenia, aes(x = Season, y = ilosc_elementow))
s + geom_boxplot()

table(jablow_zdarzenia$Season)


## Złożoność sekwencji a pora dnia
s + geom_boxplot(aes(x = pora_dnia))

# Złożoność sekwencji a pora dnia i sezon
s + geom_boxplot(aes(x = pora_dnia)) + facet_grid(.~Season)

## BOXPLOT DLA ZŁOŻONOŚCI SEKWENCJI Z PODZIAŁEM NA JEDNEGO OSOBNIKA I WIELU
## Złożoności sekwencji, a ilość osobników w obserwacji

jablow_zdarzenia <- mutate(jablow_zdarzenia, czy_grupa = Quantity>1)
table(jablow_zdarzenia$czy_grupa)


# Liczba elementów a grupa/pojedyncze przeloty
h  <- ggplot(jablow_zdarzenia, aes(x = as.factor(czy_grupa), y = ilosc_elementow))
h + geom_boxplot() 

# J/w z podziałem na sezon
h + geom_boxplot() +
  facet_grid(.~Season) + geom_jitter()



## BOXPLOT DLA ZLOZONOSCI W SYTUACJI OBECNOSCI POSCIGU LUB JEGO BRAKU


## Podsumowanie ilości poscigów w obrębie pojedynczyhc obserwacji.
# ( Tam gdzie obserwacje zostały rozbite na pojedyncze nietoperze, potrzebujemy stworzyć
# zmienną, która powie nam ile nietoperzy z tej surowej obserwacji było zaangażowane
# w pościg) 


poscigi_datetime <- group_by(jablow_zdarzenia, Date_time, Season,Night, Channel, Quantity,
                             pora_dnia)
# Musimy podsumować jakość złożoność sekwencji poszczególnych nietoperzy w obrębie 
# Pojedycznej obserwacji, więc liczymy średnią. 

poscigi_datetime_summary <- summarise(poscigi_datetime, p_sum = sum(p, na.rm=T),
                                      ilosc_elementow_mean = mean(ilosc_elementow))


# DOdajemy zmienne logiczne czy_grupa, czy_poscig, żeby móc podzielić wykres na okna
poscigi_datetime_summary <- mutate(poscigi_datetime_summary, czy_poscig = p_sum >0)
poscigi_datetime_summary <- mutate(poscigi_datetime_summary, czy_grupa = Quantity>1)

table(poscigi_datetime_summary$Season)

f <- ggplot(poscigi_datetime_summary, aes(x=czy_poscig, y=ilosc_elementow_mean))

f + geom_boxplot()
f + geom_boxplot() + facet_grid(.~ czy_grupa) + geom_jitter()

### PRZELOTY GRUPOWE RANO I WIECZOTEM

grupy_rano_wieczor <- group_by(jablow_zdarzenia, Season,Night,
                               pora_dnia, czy_grupa)
grupy_rano_wieczor_summary <- summarize(grupy_rano_wieczor, n_zdarzen =n())

m <- ggplot(grupy_rano_wieczor_summary, aes(x = czy_grupa, y = n_zdarzen))

m + geom_boxplot() + facet_grid(Season~pora_dnia) + theme_bw() + geom_jitter()


## Ogólna aktywność

o <- ggplot(jablow_zdarzenia, aes(x = Season))
o + geom_bar(aes(fill = factor(Night))) +  scale_fill_grey() + theme_bw()



## UNIKALNE SEKWENCJE

unikalne_sekwencje <- sort(table(jablow_zdarzenia$Event_V),decreasing = T)
head(unikalne_sekwencje, 100)





