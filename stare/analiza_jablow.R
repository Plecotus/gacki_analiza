## Importowanie funkcji potrzebnych do analizy

source("functions/observation_split.R")
source("functions/count_instances.R")
source("functions/u_remove.R")
source("functions/wesolowski_plot.R")



# Wczytujemy potrzebne pakiety
library(dplyr)
library(lubridate)
library(ggplot2)
library(PMCMR)




shannon <- function(x){
    s <- (-1) * sum(log(x^x))
    s
}

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
jablow$pora_dnia <- ifelse(jablow$godzina>=12, "przed wylotem", "po powrocie")

## Usuwamy zdarzenia sprzed godziny 20

jablow <- filter(jablow, godzina>=20 | godzina<7)


## Usuwamy dane zawierające oberwacje niepełne (u), bez podmianki wewnętrzych
# elementów na "/", kontroluje parametr slash

jablow <- u_remove(jablow, slash = F)

## Rozbijamy obserwacje z wieloma nietoperzami na pojedyncze obserwacje
jablow_pociete <- observation_split(jablow)
rm(jablow)



## Liczymy zdarzenia w obrębie każdego kodu

characters <-c("a","l","t","r","p","c","d","f","n","e",
               "q", "g", "h", "i", "k","o", "w", "v")
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

## MOŻNA DODAĆ EWENTUALNIE to damo dla q, i, k.!!!!

## USUWANIE EVENTÓW_V, które podminieniły sie na '', w  wyniku użycia funkcji u_remove

jablow_zdarzenia <- filter(jablow_zdarzenia, Event_V != '')


## Zamiana sezonów na nazwy miesięcy

jablow_zdarzenia <- mutate(jablow_zdarzenia, Season = factor(Season))
levels(jablow_zdarzenia$Season) <- c("maj", "lipiec", "wrzesień")

# Zamiana zabawek 0,1 na factor 'brak zabawek", "obecność zabawek"
jablow_zdarzenia <- mutate(jablow_zdarzenia, Toys = factor(Toys))
levels(jablow_zdarzenia$Toys) <-c("brak zabawek", "obecność zabawek")


## Zapisuje dane do tabeli zewnętrznej

write.csv(jablow_zdarzenia, "output/jablow_zdarzenia.csv")



### ANALIZA POWIEDZMY, ŻE STATYSTYCZNA ------

# Grupujemy po sezonach, nocach i porze dnia, żeby potem policzyć sumy dla konkretnych
# zdarzeń
jablow_zdarzenia <- group_by(jablow_zdarzenia, Season, Night, pora_dnia)

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

# Wykres aktywności dla sezonów i pór dnia

z <- ggplot(summary_jablow, aes(x = pora_dnia, y = n_observation)) +
  theme_wesolowski()

z + stat_boxplot(geom ='errorbar') +geom_boxplot() + facet_grid(. ~ Season)  +
  xlab("Pora nocy") + ylab("Liczba obserwacji") 





# Średnie ilości pościgów 
g <- ggplot(summary_jablow, aes(x = pora_dnia, y = mean_p)) + theme_wesolowski()

# Tylko za podziałem na porę dnia
g + stat_boxplot(geom ='errorbar') + geom_boxplot()

# Z podziałem tylko na sezon

g + stat_boxplot(aes(x = Season), geom ='errorbar') + geom_boxplot(aes(x = Season)) 


# Z podziałem na sezon i porę dnia
g + stat_boxplot(aes(x = pora_dnia), geom ='errorbar') +
  geom_boxplot() + facet_grid(. ~ Season) 



# Złożoność sekwencji a sezon

s <- ggplot(jablow_zdarzenia, aes(x = Season, y = ilosc_elementow)) + theme_wesolowski()
s + stat_boxplot(aes(x = Season), geom ='errorbar') + geom_boxplot()


table(jablow_zdarzenia$Season)


## Złożoność sekwencji a pora dnia
s + stat_boxplot(aes(x = pora_dnia), geom ='errorbar') + geom_boxplot(aes(x = pora_dnia))

# Złożoność sekwencji a pora dnia i sezon
s + stat_boxplot(aes(x = pora_dnia), geom ='errorbar') +
  geom_boxplot(aes(x = pora_dnia)) + facet_grid(.~Season)

## BOXPLOT DLA ZŁOŻONOŚCI SEKWENCJI Z PODZIAŁEM NA JEDNEGO OSOBNIKA I WIELU
## Złożoności sekwencji, a ilość osobników w obserwacji

jablow_zdarzenia <- mutate(jablow_zdarzenia, czy_grupa = Quantity>1)
table(jablow_zdarzenia$czy_grupa)


# Liczba elementów a grupa/pojedyncze przeloty
h  <- ggplot(jablow_zdarzenia, aes(x = as.factor(czy_grupa), y = ilosc_elementow)) +
  theme_wesolowski()
h + stat_boxplot( geom ='errorbar') + geom_boxplot() 

# J/w z podziałem na sezon
h + stat_boxplot( geom ='errorbar') + geom_boxplot() +
  facet_grid(.~Season)



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

f <- ggplot(poscigi_datetime_summary, aes(x=czy_poscig, y=ilosc_elementow_mean)) +
  theme_wesolowski()

f + stat_boxplot( geom ='errorbar') + geom_boxplot()
f + stat_boxplot( geom ='errorbar') + geom_boxplot() + facet_grid(.~ czy_grupa)

### PRZELOTY GRUPOWE RANO I WIECZOTEM

grupy_rano_wieczor <- group_by(jablow_zdarzenia, Season,Night,
                               pora_dnia, czy_grupa)
grupy_rano_wieczor_summary <- summarize(grupy_rano_wieczor, n_zdarzen =n())

m <- ggplot(grupy_rano_wieczor_summary, aes(x = czy_grupa, y = n_zdarzen)) +
  theme_wesolowski()

m + stat_boxplot( geom ='errorbar') + geom_boxplot() + 
  facet_grid(Season~pora_dnia) + theme_bw()


## Ogólna aktywność

o <- ggplot(jablow_zdarzenia, aes(x = Season)) + theme_wesolowski()
o +   geom_bar() +  scale_fill_grey() 


## WYkres zdarzeń


zabawki <- filter(jablow_zdarzenia, Night >2 & Season == 'maj' | Night >1 & Season != 'maj')
zabawki <- group_by(zabawki, Season, Night, Channel, Toys)

zabawki_summary <- summarize(zabawki, n_obs =n(), srednia_sekwencja = mean(ilosc_elementow))

t <- ggplot(zabawki_summary, aes(x = factor(Toys), y = n_obs))
t + geom_boxplot() + facet_grid(. ~ Season, scales = "free_x")+
  theme_wesolowski()

t + geom_boxplot(aes(y=srednia_sekwencja)) + facet_grid(.~Season)+
  theme_wesolowski() + ylab("Średnia długość sekwencji") + xlab("Obecność zabawek")

## UNIKALNE SEKWENCJE

unikalne_sekwencje <- sort(table(jablow_zdarzenia$Event_V),decreasing = T)
head(unikalne_sekwencje, 100)


## PRÓBA ANALIZY STATYSTYCZNEJ

pora_sezon <- factor(paste(summary_jablow$Season, summary_jablow$pora_dnia))
kruskal.test(n_observation ~ pora_sezon, data = summary_jablow)
wynik <-posthoc.kruskal.nemenyi.test(n_observation ~ pora_sezon, data = summary_jablow, 
                                     dist = "Tukey") 

x <-round(wynik$p.value, 6)
x <-round(p.adjust(wynik$p.value, n = 15), 5)
dim(x)  <- c(5,5)
x
wynik_ad <- wynik
wynik_ad$p.value <- x




### Jak analizować

## Robienie factora 6 pozimowego

# np. zabawki_sezon <- factor(paste(tabela$Toys, tabela$Season)) ## to potem idzie do 'z_podzialem_na_co"

# kruskal.test(co_testowane ~ z_podzialem_na_co, data = tabela_danych)

# potem porównianie M-W, 
# maj <-filter (tabela_danych, Season == 'maj')
# wilcox.test(co_testowane ~ z_podzialem_na_co, data = maj)


# Jak kruskal z 3 poziomami to każdy z każdym M-W.
# kruskal.test(mean_p ~ Season, data = tabela_danch)

# to potem 
# maj <- filter(tabela_danych, Season == 'maj')
# lipiec <- filter(tabela_danych, Season == 'lipiec')
# wrzesien <- filter(tabela_danych, Season == 'wrzesien')

# istotne statystycznie gdy p < 0.05/liczba_porownan (Bonferronii)
.05/15

# wilcox.test(maj$mean_p, lipiec$mean_p)
# wilcox.test(wrzesien$mean_p, lipiec$mean_p)
# wilcox.test(maj$mean_p, wrzesien$mean_p)





## ILE LITEREK

literki <- group_by(jablow_zdarzenia, Season)
liter <-summarise_each(literki,
                                 funs(
                                   sum(., na.rm = T)
                                 ), vars = -c(1:11))



boxplot(r/n_observation~Season, data = summary_jablow)
kruskal.test(r/n_observation~Season, data = summary_jablow)

maj <- filter(summary_jablow, Season == 'maj')
lipiec <- filter(summary_jablow, Season == 'lipiec')
wrzesien <- filter(summary_jablow, Season == 'wrzesień')

# istotne statystycznie gdy p < 0.05/liczba_porownan (Bonferronii)
.05/3

wilcox.test(maj$r/maj$n_observation, lipiec$r/lipiec$n_observation)
wilcox.test(wrzesien$r/wrzesien$n_observation, lipiec$r/lipiec$n_observation)
wilcox.test(maj$r/maj$n_observation, wrzesien$r/wrzesien$n_observation)



