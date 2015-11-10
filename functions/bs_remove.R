## Funkcja, która usuwa z kodów wartości oznaczone przedrostkiem b - jeżeli są na
# początku albo na końcu kodu. Jeżeli parametr slash =T, to w przypadku gdy
# wyrażenie występuje w środku podmienia
# je na '/', wg którego funckcja observation_split rozdzieli na osobne obsrewacje,
# inaczej zamienia wszystko jak leci na ""

bs_remove <- function(data, slash = F){
  
  ## Bierzemy wektor zawierający kody przelotów 
  
  kody <- data$Event_V
  
  

  ## Usuwamy zdarzenia typu "u" z początków i końców kodów
  
  kody <- gsub("([bs])","", kody)
  #kody <- gsub("([bs])+$", "", kody)
  #kody <- gsub("([bs])+$", "", kody)
  
  
  if(slash) {
  #Podmianka u.. na /
  kody <- gsub("(.b..)+", "/", kody)
  } else{
        
      kody <- gsub("(.b..)+", "", kody)
      
  }
  
  data$Event_V <- kody

  data
   
   
}