# O rozwiązaniu

## Sposób uruchamiania

make 
./interpretere < <program>

## Opis rozwiązania

Pliki rozwiązania (Interpreter i Typechecker) znajdują się w folderze src/.

### Typechecker

W monadzie ReaderT trzymana jest mapa Ident Type, w której zapisywane są typy zainicjowanych zmiennych. Typechecker przehodzi po wszystkich instrukcjach programu, sprawdzając, czy zgadzają się typy danych.

### Interpreter

W monadzie ReaderT trzymana jest mapa Ident Loc, w monadzie StateT Loc MemVal. Aby dostać wartość odpowiadającą indentyfikatorowi, konieczne jest użycie dwóch map.

## Przykłady

Przykłady użycia języka PythonScript znajdują się w folderze examples/

# Tabelka
Na 15 punktów
  01 (trzy typy) - 
        zaimplementowane ✅ 
  02 (literały, arytmetyka, porównania) 
        zaimplementowane ✅ 
  03 (zmienne, przypisanie) 
        zaimplementowane ✅ 
  04 (print) 
        zaimplementowane ✅
  05 (while, if) 
        zaimplementowane ✅ 
  06 (funkcje lub procedury, rekurencja) 
        zaimplementowane ✅ 
  07 (przez referencję / przez wartość) ✅ 
  08 (pętle for) (częściowo, bez zmiennych read-only)

Na 20 punktów
  09 (przesłanianie i statyczne wiązanie) 
        zaimplementowane ✅ 
  10 (obsługa błędów wykonania)
        zaimplementowane ✅ 
  11 (funkcje zwracające wartość) 
        zaimplementowane ✅

Na 30 punktów
  12 (4) (statyczne typowanie) 
        zaimplementowane ✅ 
  13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
        zaimplementowane ✅ 
  15 (2) (krotki z przypisaniem)
        zaimplementowane ✅ 
  17 (2-3) (funckje anonimowe)
        zaimplementowane ✅ 