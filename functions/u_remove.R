## Funkcja, która usuwa z kodów wartości oznaczone przedrostkiem u - jeżeli są na
# początku albo na końcu kodu. Natomiast jeżeli wyrażenie występuje w środku podmienia
# je na '/', wg którego funckcja observation_split rozdzieli na osobne obsrewacje.

u_remove <- function(data){
  
  ## Bierzemy wektor zawierający kody przelotów 
  
  kody <- jablow_zdarzenia$Event_V
  
  
  
  ## Usuwamy zdarzenia typu "u" z początków i końców kodów
  
  kody3 <- gsub("^u..","", kody2)
  kody3 <-  gsub("^u..","", kody3)
  kody4 <- gsub("u..$", "", kody3)
  kody5 <- gsub("u.$", "", kody4)
  
  #Podmianka u.. na /
  kody6 <- gsub(".u..", "/", kody5)
  
  
  do_pociecia <- which(regexpr("u", kody)>0)

  kody2 <-kody[do_pociecia]
   
   
}