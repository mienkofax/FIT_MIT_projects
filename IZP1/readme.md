# IZP - Základy programování

## Projekt 1 - Výpočty v tabulce

#### Popis projektu

Implementujte jednoduchý tabulkový kalkulátor. Program bude implementovat funkce vyhledání maxima, minima, funkce součtu a aritmetického průměru vybraných buněk. Tabulku ve formě textového souboru bude program očekávat na standardním vstupu. Požadovanou operaci a výběr buněk specifikuje uživatel v argumentu příkazové řádky.

#### Detailní specifikace

Program implementujte ve zdrojovém souboru proj1.c.

#### Překlad a odevzdání zdrojového souboru

**Odevzdání:** Odevzdejte zdrojový soubor proj1.c prostřednictvím informačního systému.

**Překlad:** Program překládejte s následujícími argumenty

```
$ gcc -std=c99 -Wall -Wextra -pedantic proj1.c -o proj1
```

#### Syntax spuštění

Program se spouští v následující podobě (./proj1 značí umístění a název programu a hranaté závorky reprezentují volitelnost daného argumentu programu):

```
./proj1 --help
nebo
./proj1 operace výběr_buněk
```

#### Argumenty programu:

* --help způsobí, že program vytiskne nápovědu používání programu a skončí.
* operace reprezentuje jednu z následujících operací:
	* select značí operaci, která z dané tabulky pouze vybere a následně vytiskne hodnoty daných buněk,
	* min značí vyhledání a následný tisk minimální hodnoty z daného rozsahu buněk,
	* max značí vyhledání a následný tisk maximální hodnoty z daného rozsahu buněk,
	* sum značí výpočet a následný tisk sumy hodnot všech vybraných buněk,
	* avg značí výpočet a následný tisk aritmetického průměru vybraných buněk.
* výběr_buněk reprezentuje rozsah tabulky, nad kterým má být provedena daná operace. Rozsah je definován jedním z následujících argumentů:
	* row X značí výběr všech buněk na řádku X (X > 0),
	* col X značí výběr všech buněk ve sloupci X (X > 0),
	* rows X Y značí výběr všech buněk od řádku X (včetně) až po Y (včetně). 0 < X <= Y.
	* cols X Y značí výběr všech buněk od sloupce X (včetně) až po Y (včetně). 0 < X <= Y.
	* range A B X Y značí výběr buněk od řádku A po řádek B a od sloupce X po sloupec Y (včetně daných řádků a sloupců). 0 < A <= B, 0 < X <= Y.


#### Implementační detaily

Program čte zpracovávanou tabulku ze standardního vstupu (stdin). Veškeré informace vypisuje na standardní výstup (stdout). Řádek tabulky je zakončen znakem konce řádku. Sloupce tabulky (resp. buňky na řádku) jsou odděleny jedním a více bílých znaků. Buňka může obsahovat číslo nebo text (bez bílých znaků). Maximální délka jednoho řádku je 1024 znaků.

#### Operace a jejich výstup

* Operace select vypíše seznam hodnot z daného výběru. Každá hodnota bude vypsána na jeden řádek. V případě, že hodnota je číslo, bude vypsáno formátem %.10g funkce printf. V případě, že výběr zahrnuje více řádků či sloupců, hodnoty tabulky budou zpracovávány po řádcích, tj. nejprve všechny hodnoty daného řádku zleva doprava, pak hodnoty následujícího řádku.
* Operace min, max, sum a avg pracují pouze nad buňkami obsahující číselné údaje. Funkce vypíší výsledek jako jedno číslo formátu %.10g na samostatný řádek.

**Je zakázané použít následující funkce:**

* volání z rodiny malloc a free - práce s dynamickou pamětí není v tomto projektu zapotřebí,
* volání z rodiny fopen, fclose, fscanf, ... - práce se soubory (dočasnými) není v tomto projektu žádoucí.

#### Neočekávané chování

Na chyby za běhu programu reagujte obvyklým způsobem: Na neočekávaná vstupní data, argumenty příkazového řádku, formát vstupních dat nebo chyby při volání funkcí reagujte přerušením programu se stručným a výstižným chybovým hlášením na příslušný výstup a odpovídajícím návratovým kódem.

### Příklady vstupů a výstupů

**Mějme tabulku:**

```
$ cat tabulka.txt
Maserati_Biturbo          1998    84   349000
Maserati_Coupe            2000    76   499000
Maserat_Ghibli            2004   100  1921000
Maserati_GranCabrio       2012    47  2490000
Maserati_Granturismo      2011    20  2000000
Maserati_Quattroporte_SQ4 2013     3  2978999
```

**Výpis buněk 4. řádku:**

```
$ ./proj1 select row 4 <tabulka.txt
Maserati_GranCabrio
2012
47
2490000
```

**Výpis 3. sloupce:**

```
$ ./proj1 select col 3 <tabulka.txt
84
76
100
47
20
3
```

**Výpis buněk ve 4. sloupci mezi 3. a 5. řádkem:**

```
$ ./proj1 select range 3 5 4 4 <tabulka.txt
1921000
2490000
2000000
```

**Výpočet průměrné hodnoty všech řádků ve 4. sloupci:**

```
$ ./proj1 avg col 4 <tabulka.txt
1706333.167
```

**Hledání nejmenší hodnoty ve 2. sloupci:**

```
$ ./proj1 min col 2 <tabulka.txt
1998
```

#### Hodnocení

Na výsledném hodnocení mají hlavní vliv následující faktory:

* přeložitelnost zdrojového souboru,
* formát zdrojového souboru (členění, zarovnání, komentáře, vhodně zvolené identifikátory),
* dekompozice problému na podproblémy (vhodné funkce, vhodná délka funkcí a parametry funkcí),
* správná volba datových typů a tvorba nových typů,
* vhodně zvolený algoritmus načítání a rozpoznání buněk tabulky,
* správná funkcionalita operací a
* ošetření chybových stavů.
