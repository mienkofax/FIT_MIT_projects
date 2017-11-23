# BMS - Bezdrátové a mobilní sítě

### Projekt 1

Naprogramujte v jazyku C/C++ konzolový program sloužící pro zjištění přibližné polohy pomocí okolních BTS. Vstupem bude textový soubor obsahující informaci o aktuální a okolních BTS.

Jméno souboru s informacemi načítejte jako parameter programu (jedná se o jediný vstupní parametr), soubor obsahující seznam všech BTS (bts.csv) načítejte z aktuální složky programu (./) .

Výstupem Vaší konzolové aplikace bude textový soubor "out.txt" obsahující odkaz na mapu uloženou na serveru google.com s odkazem na vypočítané souřadnice. Soubor obsahující seznam BTS bude mít formu: Seznam BTS

Výpočet vzdálenosti provádějte s využitím Hata modelu, do kterého dosaďte hodnoty ze vstupního souboru. Výška BTS sloupce - ant h je udána v metrech, přenášený výkon BTS sloupce power je udán ve ! wattech !. Zbylé hodnoty budou konstantní v následujícím znění:

Frekvence: `900 MHz`

Výška mobilní stanice: `1.2 m`

**Vzory vstupních souborů:**

```
in1.csv
in2.csv
in3.csv
```

**Ukázka požadovaného výstupu:**

```
www.google.com/maps/place/49°16'12.7"N+16°16'12.4"E
```

nebo

```
maps.google.com/maps?q=49.2702,16.2701
```

Název aplikace po aplikování příkazu make: `./p1`

Odevzdání: do WISu nahrajte zip ve formátu xlogin00.zip s okomentovaným zdrojovým souborem a makefile. 

Aplikace budou testovány na merlin.fit.vutbr.cz

**Preklad pomocou Makefile:**

```
mkdir build
cd build
make -C ../src/
```

**Preklad a spustenie pomocou CMake v adresári BMS1:**

```
skola/BMS1$ mkdir build
skola/BMS1/build$ cd build
skola/BMS1/build$ cmake ..
skola/BMS1/build$ make

skola/BMS1/build$ cd ../test/csv
skola/BMS1/test/csv$ ./../../build/src/p1 in1.csv
```

**Preklad a spustenie pomocou CMake v nadradenom adresári**
```
skola$ mkdir build
skola/build$ cd build
skola/build$ cmake ..
skola/build$ make

skola/build$ cd ../BMS1/test/csv
skola/BMS1/test/csv$ ./../../../build/BMS1/src/p1 in1.csv

skola/BMS1/test/csv$ cd ../..
skola/BMS1$ ./cmake/test/test-suite
```

**Overenie vytvoreného archívu, rozbalenie a jeho spustenie**
```
nakopirovat xlogin00.zip do skola/BMS1/xlogin00.zip

skola/BMS1/$ it_is_ok.sh xlogin00 
```
