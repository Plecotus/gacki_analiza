### GACKI ANALIZA CAŁOŚĆ

source("wczytywanie_danych.R")
## Wybór godzin pomiędzy 20:00 a 21:00 

calosc <- filter(calosc, !(kolonia == "JABLOW" & Season == "III"  & 
                                       Part_night == "wieczor" & hour(Date_time) %in% c(18,20)))


## Usuwamy dane zawierające oberwacje niepełne (u) oraz
# obserwacje duże i male (b i s), bez podmianki wewnętrzych
# elementów na "/", kontroluje to parametr slash

calosc <- u_remove(calosc, slash = F)
calosc <- bs_remove(calosc, slash = F)
calosc <- filter(calosc, Event_V != "")

## Liczymy zdarzenia w obrębie każdego kodu

characters <-c("a","v","c","l","t","o","d","f","w","n","e", "h", "g","r",
               "i", "k","q","p")

zdarzenia <- count_instances(calosc, characters)
calosc_zdarzenia <- cbind(calosc, zdarzenia)



# Zamieniamy dane o pogoni na zmienną logiczną

p <- zdarzenia$p
p[is.na(p)] <-0

calosc_zdarzenia$czy_pogon <- as.logical(p)

# ZLiczamy ilość elementów w sekwencji
calosc_zdarzenia$ilosc_elementow <- apply(calosc_zdarzenia[,16:29],1,sum, na.rm = T)
calosc_zdarzenia <- mutate(calosc_zdarzenia,
                           srednia_ilosc_elementow  = ilosc_elementow/Quantity)






# Sprawdzamy czy obserwacje z pogoniami są bardziej 
# złożone od obserwacji 2 niezależnych osobników
#sub <- filter(calosc_zdarzenia, Quantity>1)
#sub_plot <- ggplot(calosc_zdarzenia, aes(y = srednia_ilosc_elementow, x = czy_pogon))
#sub_plot + geom_boxplot() + facet_grid(.~Part_night)


### TNIEMY TABELĘ - JEDEN NIETOPERZ NA JEDNĄ OBSERWACJĘ

calosc_split <- observation_split(calosc)

# LIczymy poszczególne zdarzenia

characters <-c("a","v","c","l","t","o","d","f","w","n","e", "h", "g","r",
               "i", "k","q","p")

zdarzenia <- count_instances(calosc_split, characters)
calosc_split_zdarzenia <- cbind(calosc_split, zdarzenia)

calosc_split_zdarzenia$ilosc_elementow <- apply(calosc_split_zdarzenia[,16:29],
                                                1,sum, na.rm = T)

calosc_split_zdarzenia$shannon <- apply(as.matrix(zdarzenia), 1, function(x){
    suma <- sum(x, na.rm = T)
    prawd <- x/suma
    s <- -1 * sum(log(prawd^prawd), na.rm = T)
    s
})


shan_plot <- ggplot(calosc_split_zdarzenia, aes(x = Part_night, y = shannon))

shan_plot + geom_boxplot(aes(x = Channel)) + facet_grid(Phase~Season)






# Grupujemy po sezonach, nocach i porze dnia, żeby potem policzyć sumy dla konkretnych
# zdarzeń
calosc_split_zdarzenia <- group_by(calosc_split_zdarzenia, kolonia, Season, Night, Part_night, Channel)

### Liczymy liczbę pościgów i liczbę zdarzeń

summary_calosc <- summarise_each(calosc_split_zdarzenia,
                                 funs(
                                     sum(., na.rm = T)
                                 ), vars = -c(1:15))



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

box_act_channel <- ggplot(summary_calosc, aes( y = n_observation, x = Channel))
box_act_channel + geom_boxplot() + facet_grid(.~kolonia)

hist_act_wiecz <- ggplot(calosc[calosc$Part_night == "wieczor",], aes(x = as.numeric(after_dusk)/60))
hist_act_wiecz + geom_histogram(binwidth = 5) + facet_grid(Season ~ kolonia) +
    theme_wesolowski()

hist_act_rano <- ggplot(calosc[calosc$Part_night == "rano",], aes(x = as.numeric(till_dawn)))
hist_act_rano + geom_histogram(binwidth = 5) + facet_grid(Season ~ kolonia) + 
scale_x_continuous(limits = c(0, 150)) + theme_wesolowski()


akt_gg <- ggplot(summary_calosc, aes(y = n_observation, x = Part_night))
akt_gg + geom_boxplot() + facet_grid(kolonia~Season) + theme_wesolowski()


## Średnia ilość pogoni
pogon_gg <- ggplot(summary_calosc, aes(y = mean_p, x = Part_night))
pogon_gg + geom_boxplot() + facet_grid(kolonia~Season) + theme_wesolowski()

## Przeloty_grupowe
sub <- filter(calosc_zdarzenia, Quantity >1)
grupy <- ggplot(sub,
                aes(x = as.factor(Quantity),
                    fill=czy_pogon ))
grupy + geom_bar() + facet_grid(.~Season)



#Złożoność zachowań

calosc_split_zdarzenia <- group_by(calosc_split_zdarzenia, kolonia, Season,)

### Liczymy liczbę pościgów i liczbę zdarzeń

summary_calosc_ks <- summarise_each(calosc_split_zdarzenia,
                                 funs(
                                     sum(., na.rm = T)
                                 ), vars = -c(1:15))



## Przeliczanie na proporcje

summary_calosc_proporcje_ks <- mutate(summary_calosc_ks[,c(3,4,5, 20, 21)],
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

category<- factor(category, level = c("AVC", "LTO", "DFW", "NEH", "GR", "IKQP"))

summary_melt$category <- category
colourCount = 18
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

zdarzenia_sklad <- ggplot(summary_melt, aes(x = Season, y = value, fill = variable))
zdarzenia_sklad + geom_bar(position = "fill", stat="identity") + facet_grid(category ~ kolonia)+
    scale_fill_manual(values = getPalette(colourCount)) + theme_wesolowski()



### BRUDY
 
calosc %>% mutate(p = p) %>% filter(p == 1, Quantity == 1) -> dziwy

which(calosc$Quantity == 1 & p ==1)
calosc %>% filter(Season == "III", Phase == "C") -> dziwy
View(dziwy)
