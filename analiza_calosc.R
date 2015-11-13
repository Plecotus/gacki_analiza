### GACKI ANALIZA CAŁOŚĆ
#setwd("~/R/gacki_analiza")
source("wczytywanie_danych.R")

### ROZBUDOWYWANIE TABELI PODSTAWOWEJ ----
# Tutaj jedna obserwacja może składać się z sekwencji kilku nietoperzy
# Każda sekwencja rozdzielona jest poprzez / 



## Wybór godzin pomiędzy 20:00 a 21:00 
## BO w naszym zbiorze danych mamy też dane sprzed 20 (30 min). Chcemy się tego
## pozbyć, ze zwzględu lepszej porównywalności z wieczorem.


calosc <- filter(calosc, !(kolonia == "JABLOW" & Season == "III"  & 
                                       Part_night == "wieczor" & hour(Date_time) %in% c(18,20)))


## Usuwamy dane zawierające oberwacje niepełne (u) oraz
# obserwacje duże i male (b i s), bez podmianki wewnętrzych
# elementów na "/", kontroluje to parametr slash

## W przypadku "u" usuwamy te elementy, które były niewidoczne,
# zostawiając resztę sekwencji bez zmian.

calosc <- bs_remove(calosc, slash = F)
calosc <- u_remove(calosc, slash = F)
calosc <- filter(calosc, Event_V != "") # jeśli powstały obserwacje bez sekwencji



## Liczymy zdarzenia w obrębie każdego kodu
## Sekwencje przelotu zakodowane są jako ciąg znaków rozdzielonych kropkami
## W tym momencie liczymy wystąpienia poszczególnych ewolucji w obrębie sekwencji.

characters <-c("a","v","c","l","t","o","d","f","w","n","e", "h", "g","r",
               "i", "k","q","p")
zdarzenia <- count_instances(calosc, characters)
# Łączymy zdarzenia i podstawową tabelę w całość
calosc_zdarzenia <- cbind(calosc, zdarzenia)



# Zamieniamy dane o pogoni na zmienną logiczną
# Funkcja count_instances policzyła nam liczbę pogoni, a są obserwacje 
# z większą liczbą pogoni niż 1, a przyda nam się zminenna, czy_pogon wskazująca
# na takie konkrente obserwacje. Zmienną czy_pogoń dodajemy też do podstawowej
# tabeli, żeby nie stracić z pola widzenia sekwencji gonionego.

p <- zdarzenia$p
p[is.na(p)] <-0
calosc_zdarzenia$czy_pogon <- as.logical(p)
calosc$czy_pogon <- as.logical(p)

rm(p)

# Zliczamy ilość elementów w sekwencji
# A ponieważ jeszcze pracujemy na pełnych obserwacjach, a nie na poszczególnych
# kodach, dzielimy ilosc_elementów przez liczbę osobników w obserwacji uzyskując
# średnią złożoność.

calosc_zdarzenia$ilosc_elementow <- apply(calosc_zdarzenia[,17:30],1,sum, na.rm = T)
calosc_zdarzenia <- mutate(calosc_zdarzenia,
                           srednia_ilosc_elementow  = ilosc_elementow/Quantity)




## TO TAKIE NIE WIEM CO DO KOŃCA ################################
# Sprawdzamy czy obserwacje z pogoniami są bardziej 
# złożone od obserwacji 2 niezależnych osobników
sub <- filter(calosc_zdarzenia, Quantity>1)
sub_plot <- ggplot(calosc_zdarzenia, aes(y = srednia_ilosc_elementow, x = czy_pogon))
sub_plot + geom_boxplot() + facet_grid(.~Part_night)
##################################################################

### TNIEMY TABELĘ - JEDEN NIETOPERZ NA JEDNĄ OBSERWACJĘ ------------------

# Każda obserwacja zawierająca w kodzie / jest dzielona na dwie osobne obserwacje
# przy czym zostają zachowane pozostałe sygnatury - kolonia, sezon,  czas, czy_pogon, etc.

calosc_split <- observation_split(calosc)

# Liczymy poszczególne zdarzenia
# Ponownie, tym razem dla poszczególnych nietoperzy.

characters <-c("a","v","c","l","t","o","d","f","w","n","e", "h", "g","r",
               "i", "k","q","p")
zdarzenia <- count_instances(calosc_split, characters)

# I łączymy w jedną tabelę
calosc_split_zdarzenia <- cbind(calosc_split, zdarzenia)


rm(characters)


# Liczymy liczbę elementów w sekwencji
calosc_split_zdarzenia$ilosc_elementow <- apply(calosc_split_zdarzenia[,18:31],
                                                1,sum, na.rm = T)
# Liczymy współczynnik shannon'a dla sekwencji
calosc_split_zdarzenia$shannon <- apply(as.matrix(zdarzenia), 1, function(x){
    suma <- sum(x, na.rm = T)
    prawd <- x/suma
    s <- -1 * sum(log(prawd^prawd), na.rm = T)
    s
})

rm(zdarzenia)
# Liczymy liczbę zwrotów w sekwencji -  kolejna miara złożoności sekwencji

l <- calosc_split_zdarzenia$l
n <- calosc_split_zdarzenia$n
t <- calosc_split_zdarzenia$t
d <- calosc_split_zdarzenia$d
o <- calosc_split_zdarzenia$o
o[is.na(o)] <- 0
n[is.na(n)] <- 0
l[is.na(l)] <- 0
t[is.na(t)] <- 0
d[is.na(d)] <- 0
calosc_split_zdarzenia$n_zakretow <- l*2+ t*1 +n*1 +d*1 + o*4

c <- calosc_split_zdarzenia$c
v <- calosc_split_zdarzenia$v
c[is.na(c)] <- 0
v[is.na(v)] <- 0

calosc_split_zdarzenia$n_zakretow_vc <- l*2+ t*1 +n*1 +d*1 + o*4 + c*.5 +v *.5

rm(l); rm(n); rm(o); rm(t); rm(d); rm(c); rm(v)

### LICZENIE PODSUMOWAŃ DLA POSZCZEGÓLNYCH ETAPÓW

# Grupujemy po sezonach, nocach i porze dnia, żeby potem policzyć sumy dla konkretnych
# zdarzeń
calosc_split_zdarzenia <- group_by(calosc_split_zdarzenia, kolonia, Season, Phase,
                                   Night, Part_night, Channel, Toys_ch)

### Liczymy liczbę pościgów i liczbę zdarzeń

summary_calosc <- summarise_each(calosc_split_zdarzenia,
                                 funs(
                                     sum(., na.rm = T)
                                 ), vars = -c(1:17))



## Przeliczanie na proporcje

summary_calosc_proporcje <- mutate(summary_calosc,
                                   a = a/ilosc_elementow,
                                   v = v/ilosc_elementow,
                                   c = c/ilosc_elementow, 
                                   l = l/ilosc_elementow,
                                   t = t/ilosc_elementow, 
                                   o = o/ilosc_elementow, 
                                   d = d/ilosc_elementow,
                                   f = f/ilosc_elementow,
                                   w = w/ilosc_elementow,
                                   n = n/ilosc_elementow,
                                   e = e/ilosc_elementow,
                                   h = h/ilosc_elementow,
                                   g = g/ilosc_elementow,
                                   r = r/ilosc_elementow
                                   )

boxplot(summary_calosc_proporcje$a, summary_calosc_proporcje$v,
        summary_calosc_proporcje$c, summary_calosc_proporcje$l,
        summary_calosc_proporcje$t, summary_calosc_proporcje$o,
        summary_calosc_proporcje$d, summary_calosc_proporcje$f,
        summary_calosc_proporcje$f, summary_calosc_proporcje$w,
        summary_calosc_proporcje$n, summary_calosc_proporcje$e,
        summary_calosc_proporcje$h, summary_calosc_proporcje$g,
        summary_calosc_proporcje$r)


# Liczymy średnią liczbę pościgów
# Ilość obserwacji
summary_calosc$n_observation <- summarise(calosc_split_zdarzenia, n_observ = n())$n_observ 

# Dodajemy średnie ilości pościgów 
summary_calosc <- mutate(summary_calosc, mean_p = p/n_observation)    

# Średnie złożoności, czyli średnia ilość elementów w pojedynczej obserwacji
summary_calosc$srednia_sekwencja <- summarise(calosc_split_zdarzenia,
                                              srednia_sekwencja = mean(ilosc_elementow))$srednia_sekwencja
summary_calosc$sredni_shannon <- summarise(calosc_split_zdarzenia,
                                              sredni_shannon = mean(shannon))$sredni_shannon


## OGÓLNA AKTYWNOŚĆ


# Histogram aktywności wieczorem

hist_act_wiecz <- ggplot(calosc_split_zdarzenia[calosc_split_zdarzenia$Part_night == "wieczor",], aes(x = as.numeric(after_dusk)/60))

jpeg("images/aktywnosc_wieczorna_hit.jpeg", 1200, 800, quality = 100)
hist_act_wiecz + geom_histogram(binwidth = 5) + facet_grid(Season ~ kolonia) +
    theme_wesolowski() + xlab("Minuty po zachodzie słońca") + ylab("Liczba obserwacji")
dev.off()


# Histogram aktywności rano
 
hist_act_rano <- ggplot(calosc_split_zdarzenia[calosc_split_zdarzenia$Part_night == "rano",], aes(x = as.numeric(till_dawn)))

jpeg("images/aktywnosc_rano_hist.jpeg", 1200, 800, quality = 100)
hist_act_rano + geom_histogram(binwidth = 5) + facet_grid(Season ~ kolonia) + 
scale_x_continuous(limits = c(0, 150)) + 
  theme_wesolowski() + xlab("Minuty do wschodu słońca") + ylab("Liczba obserwacji")
dev.off()

#######!!!!!!!!!! TUTAJ TRZEBA SIĘ ZASTANOWIĆ CZY TAK TO PRZEKAZYWAĆ? CZY NIE UŚREDNIĆ
# KANAŁÓW

summary_calosc <- group_by(summary_calosc, kolonia, Season, Phase, Night, Part_night)
summary_calosc2 <- summarise_each(summary_calosc,
                                  funs(
                                    mean(., na.rm = T)
                                  ), vars = -c(1:7))
  

box_act_channel <- ggplot(summary_calosc, aes( y = n_observation, x = Channel))
box_act_channel + geom_boxplot() + facet_grid(Season~kolonia, scale = "free") 

wilcox.test(n_observation~Channel, summary_calosc)
## AKTYWNOŚĆ,BOXPLOTY
akt_gg <- ggplot(summary_calosc2, aes(y = n_observation, x = Part_night))

wilcox.test(n_observation~Part_night, summary_calosc2)

jpeg("images/aktywnosc_boxploty.jpeg", 1200, 800, quality = 100)
akt_gg + geom_boxplot() + facet_grid(kolonia~Season) + theme_wesolowski() + 
      xlab("Pora nocy") + ylab(" Liczba obserwacji")
dev.off()



## Proporcje sekwencji z pogonią do wszystkich sekwencji - UWAGA JAK WYŻEJ

pogon_gg <- ggplot(summary_calosc2, aes(y = mean_p, x = Part_night))

jpeg("images/pogonie_boxploty.jpeg", 1200, 800, quality = 100)
pogon_gg + geom_boxplot() + facet_grid(kolonia~Season) + theme_wesolowski()+
  xlab("Pora nocy") + ylab ("Proporcja sekwencji z pogoniami")
dev.off()



## Przeloty_grupowe
sub <- filter(calosc_zdarzenia, Quantity >1)
grupy <- ggplot(sub,
                aes(x = factor(czy_pogon, levels = c("FALSE", "TRUE"), labels = c("Brak", "Pogoń"))))

jpeg("images/pogon_brakpogoni_2i_wiecej.jpeg", 1200, 800, quality = 100)
grupy + geom_bar() + theme_wesolowski() + xlab("Obecność pogoni") +
  ylab("Liczba obserwacji")
dev.off()


table((sub$czy_pogon))


###



shan_plot <- ggplot(calosc_split_zdarzenia, aes(x = Part_night, y = shannon))

jpeg("images/shanon_kolonia_sezon_box.jpeg", 1200, 800, quality = 100)
shan_plot + geom_boxplot() + facet_grid(kolonia~Season) +
  ylab("H'") + xlab("Pora nocy") + theme_wesolowski()
dev.off()

elements_pogon <- ggplot(
  calosc_split_zdarzenia, aes(
    x = factor(czy_pogon, levels = c("FALSE", "TRUE"), labels = c("Brak", "Pogoń")),
    y = ilosc_elementow))

jpeg("images/liczba_elemetow_przy_pogoni.jpeg", 1200, 800, quality = 100)
elements_pogon + geom_boxplot() + facet_grid(kolonia~Season) + theme_wesolowski() + 
  ylim(c(0, 5)) + ylab("Liczba elementów w sekwencji") + xlab("Obecność pogoni")
dev.off()



shan_pogon <- ggplot(
  calosc_split_zdarzenia,
  aes(x = factor(czy_pogon, levels = c("FALSE", "TRUE"), labels = c("Brak", "Pogoń")),
      y = shannon))

jpeg("images/shannon_przy_pogoni.jpeg", 1200, 800, quality = 100)
shan_pogon + geom_boxplot() + theme_wesolowski() + xlab("Obecność pogoni") + 
  facet_grid(kolonia~Season) + ylab("H'")
dev.off()

wilcox.test(calosc_split_zdarzenia$shannon ~ calosc_split_zdarzenia$czy_pogon)

zwroty_pogon <- ggplot(calosc_split_zdarzenia, aes(x = czy_pogon, y = n_zakretow))
zwroty_pogon  + geom_boxplot() + facet_grid(Part_night ~ kolonia) + ylim(c(0, 5))

table(calosc_zdarzenia$czy_pogon)


#Złożoność zachowań
calosc_split_zdarzenia <- group_by(calosc_split_zdarzenia, kolonia, Season)

### Liczymy liczbę pościgów i liczbę zdarzeń

summary_calosc_ks <- summarise_each(calosc_split_zdarzenia,
                                 funs(
                                     sum(., na.rm = T)
                                 ), vars = -c(1:17))



## Przeliczanie na proporcje

summary_calosc_proporcje_ks <- mutate(summary_calosc_ks,
                                   a = a/ilosc_elementow,
                                   v = v/ilosc_elementow,
                                   c = c/ilosc_elementow, 
                                   l = l/ilosc_elementow,
                                   t = t/ilosc_elementow, 
                                   o = o/ilosc_elementow, 
                                   d = d/ilosc_elementow,
                                   f = f/ilosc_elementow,
                                   w = w/ilosc_elementow,
                                   n = n/ilosc_elementow,
                                   e = e/ilosc_elementow,
                                   h = h/ilosc_elementow,
                                   g = g/ilosc_elementow,
                                   r = r/ilosc_elementow
)

summary_melt <- melt(summary_calosc_ks[,1:20], id.vars = c("kolonia", "Season"))


category <- vector("character")
for (i in as.numeric(summary_melt$variable)){
    if(i %in% 1:3) category <- append(category, "AVC")
    if(i %in% 4:6) category <- append(category,"LTO")
    if(i %in% 7:9) category <- append(category,"DFW")
    if(i %in% 10:12) category <- append(category, "NEH")
    if(i %in% 13:14) category <- append(category,"GR")
    if(i %in% 15:18) category <- append(category,"IKQP")
}

category <- factor(category, level = c("AVC", "LTO", "DFW", "NEH", "GR", "IKQP"),
                   labels = c("Horyzontalnie", "Zakręt", "W dół", "W górę", "Zatrzymanie",
                   "Pozostałe"))

summary_melt$category <- category
colourCount = 18
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

zdarzenia_sklad <- ggplot(summary_melt, aes(x = Season, y = value, fill = variable))

jpeg("images/barplot_elementy.jpeg", 1200, 800, quality = 100)
zdarzenia_sklad + geom_bar(position = "stack", stat="identity") +
  facet_grid(category ~ kolonia,  scales = "free")+
    scale_fill_manual(values = getPalette(colourCount)) + theme_wesolowski()+
  xlab("Faza sezonu") + ylab("Liczba zdarzeń")
dev.off()

jpeg("images/barplot_elementy_fill.jpeg", 1200, 800, quality = 100)
zdarzenia_sklad + geom_bar(position = "fill", stat="identity") +
  facet_grid(category ~ kolonia,  scales = "free")+
  scale_fill_manual(values = getPalette(colourCount)) + theme_wesolowski()+
  xlab("Faza sezonu") + ylab("Proporcja zdarzeń")
dev.off()



zdarzenia_sklad_grupy <- ggplot(summary_melt, aes(x = category, y = value))

jpeg("images/kategorie_kolonie.jpeg", 1300, 800, quality = 100)
zdarzenia_sklad_grupy + geom_bar(stat = "identity", aes(fill = variable))+
  scale_fill_manual(values = getPalette(colourCount)) + theme_wesolowski() +
  facet_grid(.~kolonia) + xlab("Kategoria ruchu") + ylab("Liczba zdarzeń")
dev.off()



# Zachowania z zabawkami

zabawki_j <- ggplot(filter(calosc_split_zdarzenia, kolonia == "JABLOW"), aes(x = Phase, y = shannon))
zabawki_j + geom_boxplot() + facet_grid(Season~Toys_ch) + ylim(c(0,2))

zabawki_j + geom_bar(aes(x = czy_pogon), stat = "identity")+ facet_grid(Season~Toys_ch)
zabawki_k <- ggplot(filter(calosc_split_zdarzenia, kolonia == "KRAJANOW"), aes(x = Phase, y = shannon))
zabawki_k + geom_boxplot() + facet_grid(Season~Toys_ch) + ylim(c(0,2))


zabawki_j_a <- ggplot(filter(summary_calosc, kolonia == "JABLOW", Season == "III"), aes(x = Phase, y = n_observation))
zabawki_j_a + geom_boxplot() + facet_grid(Part_night~Toys_ch)  + geom_jitter()
# Aktywność z zabawkami

summary_calosc$Toys_ch <-factor(summary_calosc$Toys_ch, levels = c("FALSE", "TRUE"), labels = c("Kanał bez zabawek",
                                                                                 "Kanał z zabawkami"))

zabawki_akt_j <- ggplot(filter(summary_calosc, kolonia == "JABLOW"), aes(x = Phase, y = n_observation))
jpeg("images/aktywnosc_zabawki_jablow.jpeg", 1200, 800, quality = 100)
zabawki_akt_j + geom_boxplot() +
  facet_grid(Season~Toys_ch, scale = "free") + theme_wesolowski()+
  xlab("Etap obserwacji") + ylab("Liczba obserwacji")
dev.off()


## TO WYKRES Z WCZORAJ A PROPOS ROZMOWYH NA FB
zabawki_akt_k + geom_boxplot(aes(x = Season)) + facet_grid(.~Toys_ch, scale = "free") + theme_wesolowski()

zabawki_akt_k <- ggplot(filter(summary_calosc, kolonia == "KRAJANOW"), aes(x = Phase, y = n_observation))

jpeg("images/aktywnosc_zabawki_krajanow.jpeg", 1200, 800, quality = 100)
zabawki_akt_k + geom_boxplot() + facet_grid(Season~Toys_ch, scale = "free") + theme_wesolowski()+
  xlab("Etap obserwacji") + ylab("Liczba obserwacji")
dev.off()


### Statystyka

#  CZY KANAŁY RÓŻNIĄ SIE MIĘDZY SOBĄ
dane_melt <- select(summary_calosc,kolonia, Season, Phase, Night, Part_night, Channel, n_observation)
names(dane_melt)[6:7] <- c("variable", "value")
dane <- dcast(dane_melt, kolonia+Season+Phase+Night +Part_night~variable)

cor.test(j$ch1, j$ch2, method = "spearman")
k <- filter(dane, kolonia == "KRAJANOW")
j <- filter(dane, kolonia == "JABLOW")







### BRUDY
 
calosc %>% mutate(p = p) %>% filter(p == 1, Quantity == 1) -> dziwy

which(calosc$Quantity == 1 & p ==1)
calosc %>% filter(Season == "III", Phase == "C") -> dziwy
View(dziwy)
