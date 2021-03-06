# Exercici 1. Avaluació d’expressions aritmètiques.

S'han realitzat tots els exercicis de la pràctica, incluint-hi l'opcional.

## Estructura general dels exercicis

Tots els exercicis compten amb la mateixa estructura.

Carpeta app:
* **Lexer.x:** És el fitxer on definim l'analitzador lèxic.
* **Parser.y:** És el fitxer on definim l'analitzador sintàctic
* **ParserData.hs:** És el fitxer amb les definicions de tipus i funcions auxiliars que utilitzarem en la part de l'anàlisi sintàctic. Ho hem decidit fer així perque si no el parser contenia massa lògica i feia més dificil la seva lectura i compensió.
* **AlexUserState:** Es defineix un estat dins la monada Alex que és el que mantindrà la taula de tipus.

Carpeta test amb els testos de cada un dels exercicis, s'han realitzat amb la llibreria Tasty.

## Requeriments Tècnics:

### Alex
* L’analitzador lèxic. Ve juntament amb la plataforma de haskell. En el desenvolupament, s’ha provat amb les versions 3.2.4.

### Happy
* L'analitzador sintàctic emprat. També ve conjuntament amb la plataforma de Haskell. S'ha probat amb la versió 1.20.0

### GHC
* Compilador de haskell. S’ha utilitzat la versió 8.10.4.

### Cabal
* Gestor de paquets de haskell. Ha sofert canvis dràstics en l’últim any, pel que es necessari una versió superior com a mínim a 3.4.0. Per a actualitzar el cabal donat un cabal instal·lat, utilitza la comanda cabal new-install Cabal cabal-install. 

## Execució:
Per tal d'executar els exercicis, moures a la carpeta  de l'exercici i executar:
```
make all
cabal v2-run calc-hs.cabal
```
Per tal d'executar els tests:
```
cabal v2-test
```

# Link Repo
https://github.com/sergisi/alex-hs

# Autors
Sergi Simón Balcells
Joaquim Picó Mora