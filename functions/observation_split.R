### Funkcja, która rozbija obserwacje zawierające mnogie przeloty nietoperzy
# na osobne obserwacje.


observation_split <- function(data){
  
  ## Bierzemy wektor zawierający kody przelotów i szukamy znaków "/", według 
  # których będziemy rozdzielać obserwacje
 
  kody <- data$Event_V
  
  do_pociecia <- which(regexpr("/", kody)>0)
  
  #Tworzymy nowe puste tabele danych o tej samej iloście kolumn co dane wejsciowe
  pusta_tabela <- data[-c(1:length(data[,1])),]
  nowe_obserwacje <- pusta_tabela
  
  
  
  for(i in do_pociecia){
  ## Wyciągamy obserwację z tabeli
    obserwacja <- data[i,]
  ## Tniemy kody przelotów
    ciecie <- strsplit(obserwacja$Event_V, "/")
  ## Podmianka kodów
    wyjscie <- pusta_tabela
    for(item in ciecie[[1]]){
      nowe <- obserwacja
      nowe$Event_V <- item
      wyjscie <-rbind(wyjscie, nowe)
      
    }
    nowe_obserwacje <- rbind(nowe_obserwacje, wyjscie)
  }
  
  ## Czyścimy z pociętych obserwacji
  dane <- data[-do_pociecia,]
  
  ## Dodajemy nowe_obserwacje
  dane <- rbind(dane, nowe_obserwacje)
}
