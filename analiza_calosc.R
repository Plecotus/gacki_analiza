### GACKI ANALIZA CAŁOŚĆ



# WCZYTYWANIE FUNKCJI i BIBLIOTEK


source("functions/observation_split.R")
source("functions/count_instances.R")
source("functions/u_remove.R")
source("functions/bs_remove.R")
source("functions/get_sun.R")

library(dplyr)
library(lubridate)
library(ggplot2)


# Wczytywanie dancyh o słońcu

sun <- get_sun("input/wschody_zachody_2012.csv")
sun <- tbl_df(sun)


# WCZYTUJEMY OBIE TABELE

jablow <- read.csv2("input/jablow_calosc.csv", as.is = TRUE)
krajanow <- read.csv2("input/krajanow_calosc.csv", as.is = TRUE)

jablow <- tbl_df(jablow)
krajanow <- tbl_df(krajanow)

calosc <- bind_rows(krajanow, jablow)


rm(jablow)
rm(krajanow)


## Zmieniam daty na format R - patrz lubridate
calosc$Date_time <- dmy_hms(paste(calosc$Data, calosc$Time))

# Ustalamy porę dnia (wieczór/rano)
calosc$godzina <- hour(calosc$Date_time)
calosc$Part_night <- factor(ifelse(calosc$godzina>=12, "wieczor", "rano"))
calosc$Part_night <- relevel(calosc$Part_night, "wieczor")

# Czyścimy tabelę ze zbędnych kolumn 
calosc <- select(calosc, Season, Phase, Date_time,Channel, Night, Part_night,
                 Event_V, Toys, Quantity)

# Dodajemy dane o słońcu 
calosc$date <- as.Date(calosc$Date_time)
calosc <- left_join(calosc, sun, by ="date" )

calosc2 <- mutate(calosc,after_down = Date_time - sunset,
                 till_dusk = sunrise - Date_time)

plot(calosc$date,calosc$after_down )
calosc %>% filter(Part_night == "wieczor")%>% select(after_down) %>% as.numeric() %>% hist()


hist(as.numeric(calosc2$after_down[calosc$Part_night=="wieczor"])*60)

hist(as.numeric(calosc2$till_dusk[calosc$Part_night=="rano"]))

## Usuwamy dane zawierające oberwacje niepełne (u) oraz
# obserwacje duże i male (b i s), bez podmianki wewnętrzych
# elementów na "/", kontroluje to parametr slash

calosc <- u_remove(calosc, slash = F)
calosc <- bs_remove(calosc, slash = F)

## Liczymy zdarzenia w obrębie każdego kodu

characters <-c("a","l","t","r","p","c","d","f","n","e", "u", "g", "h", "i",
               "k","o", "w", "v")

zdarzenia <- count_instances(calosc, characters)









p <- zdarzenia$p
p[is.na(p)] <-0
x <- table(as.logical(calosc$Quantity-1), as.logical(p))

chisq.test(x)
### BRUDY
x <- 
calosc %>% mutate(p = p) %>% filter(p == 1, Quantity == 1) -> dziwy

which(calosc$Quantity == 1 & p ==1)
