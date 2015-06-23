## Funckcja zlicza jakie występowanie poszczególnych elementów w obrębie zdarzenia


count_instances <- function(data, characters){
  
  kody <- data$Event_V
  
  
  ## Pętala po wszystkich znaach zawartych w parametrze 'characters'.
  # Dla każdej litery sprawdza ilość wystąpień w danej obserwacji i zwraca do 
  # wektora pod tą samą nazwą.
  
  for(char in characters){
    
    assign(char, NULL)
    for( kod in kody){
      indeksy_wystapien <- gregexpr(char, kod)[[1]]
      
      assign(char, append(get(char), ifelse(indeksy_wystapien[1]==-1,
                                            NA, length(indeksy_wystapien))))
    }
    
  }
  
  ## Montuje wszystkie wektory w jedną kolumnę. Każda kolumna nazywa się tak jak
  # liczony kod
  
  output <- get(characters[1])
  for( char in characters[-1]){
    output <- cbind(output, get(char))
  }
  
  
  output <- data.frame(output)
  names(output) <- characters
  
  # Funkcja zwraca całą tabelę, która zawiera tyle samo obserwacji co dane wejściowe
  output
}
