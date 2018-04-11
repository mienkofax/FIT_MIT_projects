# Funkcionálny projekt SIMPLIFY-BKG do predmetu FLP 2017/2018

## O programe
Program slúži pre odstránenie zbytočných symbolov z bezkontextovej gramatiky (BKG). Algoritmus na odstránenie zbytočných symbolov, podľa ktorého program pracuje, je dostupný v opore k predmetu TIN. Jedná sa o algoritmus 4.3.

## Použitie

### Preklad
```
make - preklad aplikácie
make zip - vytvorenie archívu pre odovzdanie s potrebnými súbormi
```

### Spustenie
```
./simplify-bkg [-i] [-1] [-2] [filename]
```
* -i    rozparsovanie a vypísanie gramatiky vo vstupnom súbore
* -1    výpis gramatiky po prvom kroku v algoritmu 4.3
* -2    výpis gramatiky po druhom kroku v algoritmu 4.3

## Testy
Súčasťou projektu sú testy na overenie funkčnosti projektu. Testy sa skladajú z troch častí:
* overenie spracovania argumentov
* detekcia nesprávnej gramatiky vo vstupnom súbore
* výstupy so správnymi gramatikami pre jednotlivé prepínače -i, -1, -2

### Spustenie testov
Pre spustenie testov je potrebné vytvoriť archív na odovzdanie a spustiť skript s požadovaným loginom. Podrobnejší popis testov je priložený v zložke.
```
make zip
./it_is_ok.sh xtisov00
```

Testy boli vytvorené spolu s Klárou Nečasovou (xnecas24). Informácie o tom, kto na čom pracoval, sú dostupné v súbore ***readme-tests.md*** v zložke.

