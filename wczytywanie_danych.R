# WCZYTYWANIE FUNKCJI i BIBLIOTEK


source("functions/observation_split.R")
source("functions/count_instances.R")
source("functions/u_remove.R")
source("functions/bs_remove.R")
source("functions/get_sun.R")
source("functions/wesolowski_plot.R")

library(dplyr)
library(lubridate)
library(ggplot2)
library(lattice)
library(tidyr)
library(reshape2)
library(RColorBrewer)


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


## DOdawanie zmiennej "kolonia"

calosc$Season <- toupper(calosc$Season)

calosc<- separate(calosc,Season, c("kolonia", "Season"), "_")
calosc$Season[calosc$Season == "1"] <- "I" 
calosc$Season[calosc$Season == "2"] <- "II" 
calosc$Season[calosc$Season == "3"] <- "III" 



## Poprawiamy kanały 
calosc$Channel[calosc$Channel == "Ch1"] <- "ch1"
calosc$Channel[calosc$Channel == "KAM2"] <- "ch2"
calosc$Channel[calosc$Channel == "KAM4"] <- "ch1"


calosc$Toys_ch <- vector(length = dim(calosc)[1])
calosc$Toys_ch[calosc$Season %in% c("I", "III") & calosc$Channel == "ch2"] <- TRUE
calosc$Toys_ch[calosc$Season %in% c("II") & calosc$Channel == "ch1"] <- TRUE


## Zmieniam daty na format R - patrz lubridate
calosc$Date_time <- dmy_hms(paste(calosc$Data, calosc$Time))
calosc$Date_time <- calosc$Date_time -  minutes(60)

# Ustalamy porę dnia (wieczór/rano)
calosc$godzina <- hour(calosc$Date_time)
calosc$Part_night <- factor(ifelse(calosc$godzina>=12, "wieczor", "rano"))
calosc$Part_night <- relevel(calosc$Part_night, "wieczor")

# Czyścimy tabelę ze zbędnych kolumn 
calosc <- select(calosc, kolonia, Season, Phase, Date_time,Channel, Night, Part_night,
                 Event_V, Toys, Toys_ch, Quantity)

# Dodajemy dane o słońcu 
calosc$date <- as.Date(calosc$Date_time)
calosc <- left_join(calosc, sun, by ="date" )

calosc <- mutate(calosc,after_dusk = Date_time - sunset,
                 till_dawn = sunrise - Date_time)

rm(sun)
