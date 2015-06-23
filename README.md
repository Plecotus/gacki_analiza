# Gacki - Jabłów - analiza

## Do zrobienia:




#Uwagi 



## Log:

23 czerwca:

* wrzucilam "jablow_calosc.csv" z poprawionymi danymi "Toys" - tylko zdarzenia z kanalow, na kltorych byly zabowki maja wartosci dodatnie, reszta to "0"
* podzielilismy na rano i wieczor
* wstepna eksploracja z ggplotem
* naprawiona funkcja liczaca zdarzenia

3 czerwca: 

* uzupełnione kody zdarzeń w skrypcie "analiza_jablow"
* dodalam linie kodu "jablow <- jablow[, 1:8]" bo z tabeli "jablow_calosc.csv" importowalo sie ponad 1000 kolumn
* wyeksportowalam tabele zewnetrzna "jablow_zdarzenia" z uzupełnionymi kodami ewolucji

2 czerwca:

* [Przepiora] zaktualizowany plik "jablow_calosc.csv" - plik zawiera tylko surowe dane

* zmodyfikowana funkcja u_remove - tak, że można kontrolować wstawianie "/" do kodu
* wrzuciłem swoje dane do folderu wsadowego
* zmieniłem nazwę pliku analizy - na analiza_jablow.R i dodałem kopię dla krajanowa

31 maja:

* gotowa funkcja do rozdzielania mnogich obserwacji
* gotwa funkcja licząca zdarzenia z kodów
* gotowa funkcja do usuwania zdarzeń u
* w pliku analizy dopiero wczytanie danych i wstępne policzenie ewolucji


