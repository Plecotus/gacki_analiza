# Gacki - Jabłów - analiza


## Do zrobienia:




### To było do sprawdzenia, ale i tak będziemy to robić od nowa - nie zmergowałeś wersji z 24.07
### 24.07.2014 Sprawdź proszę Zieleń, czy dobrze rozpisałam ciało testu satystycznego dla:

* liczba zdarzeń a rodzaj przelotu, pora dnia i sezon (332 linia)
* liczba elementów w sekwencji a rodzaj przelotu (262)
* złozoność sekwencji a pora dnia (230)


## Co mamy

29.10.2015

* poprawienie wczytywania danych  -  strefy czasowe.

24.07.2015

* Dodałam linie testow statystycznych, sprawdź proszę Zieleń, czy dobrze!


* powrównianie ogólnej aktywności (z)

* porównanie średnich ilości pościgów pomiędzy sezonami 
(g, n = 84 - 21 dni x 2 pory x 2 kanały) teraz jest /2
* porównanie średnich ilości pościgów pomiędzy porami dnia 
(g, n = 84 - 21 dni x 2 pory x 2 kanały)
* porównanie srednich ilości poscigów pomiędzy sezonami i porami dnia 
(g, n = 84 - 21 dni x 2 pory x 2 kanały)




* porównanie złożoności sekwencji a sezon (s, n = 4123)
* porównanie złożoności sekwencji a pora dnia
* porównanie złożoności sekwencji a pora dnia i sezon

* porównianie złożoności sekwencji w przypadku obecności pojedynczego osobnika 
albo grupy osobników (h, n= 4123, czyli liczba wszystkich zarejestrowanych nietoperzy,
pojedycze 2483, w grupie 1640)



* ogólna aktywność z podziałem na noce (o)


* porównianie złożoności sekwencji a obecność pogoni/jej braku(f)
* porównianie złożoności sekwencji a obecność pogoni/jej braku, a obecność grupy



* porównianie przelotów w grupach dla sezonu i pory dnia (m)



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


