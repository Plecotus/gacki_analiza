## Funkcja, która usuwa z kodów wartości oznaczone przedrostkiem u - jeżeli są na
# początku albo na końcu kodu. Natomiast jeżeli wyrażenie występuje w środku podmienia
# je na '/', wg którego funckcja observation_split rozdzieli na osobne obsrewacje.

u_remove <- function(data){
  
  ## Bierzemy wektor zawierający kody przelotów 
  
  kody <- data$Event_V
  
  
  
  ## Usuwamy zdarzenia typu "u" z początków i końców kodów
  
  kody <- gsub("^u..","", kody)
  kody <-  gsub("^u..","", kody)
  kody <- gsub("u..$", "", kody)
  kody <- gsub("u.$", "", kody)
  
  #Podmianka u.. na /
  kody <- gsub(".u..", "/", kody)
  
  
  data$Event_V <- kody

  data
   
   
}