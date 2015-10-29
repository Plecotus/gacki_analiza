### Funkcja, która pobiera plik zawierający godziny wschodów i zachodów słońca, 
## i wypluwa bardziej czytelną tabelę. Formatowanie pliku z danymi
## o słońcu, jak ze strony http://www.sunearthtools.com/solar/sunrise-sunset-calendar.php
## (np. 01 Sun; sunrise,
## sunset, I 2012)


## dataset - dane z bramki, sun_file - plik z godzinami wschodów i zachodów
get_sun <- function(sun_file){
    
    
    #### Wczytuje dane o słońcu
    sun_df <- read.csv2(sun_file, header = TRUE, 
                        colClasses = "character")[,1:4]
    # Łączę komórki dat ( dzień z miesiącem i rokiem)
    date_char <- paste(substr(sun_df$Date, 1,2), sun_df$Month_year)
    
    
    # konwertuje godziny wschodów i zachodów do klasy POSIXct    
    rise <- as.POSIXct(paste(sun_df$Sunrise, date_char), format = "%T %d %b-%y", tz= "GMT")
    set <- as.POSIXct(paste(sun_df$Sunset, date_char), format = "%T %d %b-%y", tz= "GMT")
        
    # Montuję tabelę wyjściową
    date <- as.Date(date_char, format = "%d %b-%y")
    sun_data <- data.frame(date=date, sunset = set, sunrise  =rise)
    sun_data
    
    }