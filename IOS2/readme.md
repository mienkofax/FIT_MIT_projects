# IOS - Operační systémy - projekt 2

Implementujte v jazyce C modifikovaný synchronizační problém Building H20 (můžete se inspirovat knihou The Little Book of Semaphores). Existují dva typy procesů, kyslík a vodík. Aby se mohly spojit do molekuly vody, musí se počkat na příchod příslušných atomů, tj. jeden atom kyslíku a dva atomy vodíku. Poté se spojí do molekuly vody a proces se opakuje.

### Detailní specifikace úlohy

#### Spuštění
```
$ ./h2o N GH GO B
```

kde

* **N** je počet procesů reprezentujících kyslík; počet procesů reprezentujících vodík bude vždy ```2*N, N > 0```.
* **GH** je maximální hodnota doby (v milisekundách), po které je generován nový proces pro vodík. ```GH >= 0 && GH < 5001```.
* **GO** je maximální hodnota doby (v milisekundách), po které je generován nový proces pro kyslík. ```GO >= 0 && GO < 5001```.
* **B** je maximální hodnota doby (v milisekundách) provádění funkce bond (viz sekce * Popis procesů). ```B >= 0 && B < 5001```.
* Všechny parametry jsou celá čísla.

#### Implementační detaily

* Každému atomu odpovídá jeden proces.
* Hlavní proces vytváří dva pomocné procesy, každý pro generování procesů atomů stejné kategorie
	* jeden proces generuje procesy pro vodík; každý nový proces je generován po uplynutí náhodné doby z intervalu <0, GH>; vygeneruje 2*N procesů.
	* další proces generuje procesy pro kyslík; každý nový proces je generován po uplynutí náhodné doby z intervalu <0, GO>; vygeneruje N procesů.
	* postupně tedy vznikne hlavní proces, dva pomocné procesy pro generování atomů a 3*N procesů atomů.
* Každý proces hydrogen i oxygen bude interně identifikován celým číslem I, začínajícím od 1. Číselná řada je pro každou kategorii atomů zvlášť.
* Každý proces atomu vykonává své akce a současně zapisuje informace o akcích do souboru s názvem h2o.out.
	* Přístup k výstupnímu zařízení (zápis informací) musí být výlučný; pokud zapisuje jeden proces a další chce také zapisovat, musí počkat na uvolnění zdroje.
	* Součástí výstupních informací o akci je pořadové číslo A prováděné akce (viz popis výstupů). Akce se číslují od jedničky.
* Použijte sdílenou paměť pro implementaci čítače akcí a sdílených proměnných nutných pro synchronizaci.
* Použijte semafory pro synchronizaci procesů.
* Nepoužívejte aktivní čekání (včetně cyklického časového uspání procesu) pro účely synchronizace.
* Procesy atomů, které již utvořily molekulu, čekají na všechny ostatní procesy; všechny procesy atomů se ukončí současně.
* Hlavní proces čeká na ukončení všech vytvořených procesů. Poté se ukončí s kódem (exit code) 0.
* Budete-li potřebovat generovat unikátní klíč, je vhodné použít funkci ftok.
* Další funkce a systémová volání: **fork, wait, shmat, semctl, semget, shmget, sem_open, usleep, ...**

#### Chybové stavy

* Pokud některý ze vstupů nebude odpovídat očekávanému formátu nebo bude mimo povolený rozsah, program vytiskne chybové hlášení na standardní chybový výstup, uvolní všechny dosud alokované zdroje a ukončí se s kódem (exit code) 1.
* Pokud selže systémové volání, program vytiskne chybové hlášení na standardní chybový výstup, uvolní všechny alokované zdroje a ukončí se s kódem (exit code) 2.

#### Popis procesů a jejich výstupů

**Poznámka k výstupům:**

* **A** je pořadové číslo prováděné akce,
* **NAME** je zkratka kategorie příslušného procesu, tj. H nebo O,
* **I** je interní identifikátor procesu v rámci příslušné kategorie,
* Při vyhodnocování výstupu budou ignorovány mezery a tabelátory.

**Proces atomu (oxygen i hydrogen)**

1. Po spuštění tiskne ```A: NAME I: started```.
2. Ověřuje stav za následujících podmínek:
	* pokud se zrovna tvoří jiná molekula, proces čeká, dokud nebude schopen provést bod ```2b```
	* pokud se zrovna netvoří jiná molekula, proces ověří, zda je k dispozici potřebný počet volných atomů příslušných typů:
		* pokud ne, tiskne ```A: NAME I: waiting``` a čeká na chybějící procesy
		* pokud ano (je k dispozici dvakrát hydrogen a jednou oxygen), tiskne ```A: NAME I: ready``` a začne proces tvorby molekuly (tento proces a procesy, které tiskly waiting, přechází na bod 3)
3. Jakmile jsou k dispozici tři vhodné atomy, zavolá každý z těchto tří procesů funkci ```bond```, v které:
	* tiskne ```A: NAME I: begin bonding```
	* uspí proces na náhodnou dobu z intervalu ```<0, B>```
4. Jakmile se atomy spojí do molekuly, tj. všechny tři procesy dokončily funkci ```bond```, tak poté tiske každý z těchto procesů ```A: NAME I: bonded```.
5. Procesy atomů se ukončí současně, tj. čekají, až se vytvoří molekuly ze všech atomů. Poté každý proces atomu tiskne ```A: NAME I: finished```.
6. **Omezující podmínky:** Pokud se tvoří molekula (body ```2.b.II, 3``` a ```4```), nesmí další procesy provádět totéž, tj. v jednom okamžiku může vznikat pouze jedna molekula. Další atomy musí počkat, dokud se úplně nedokončí předchozí molekula, tj. skončí provádění bodu 4. Toto omezení platí pro body 2.b, 3 a 4 (při tvorbě molekuly se nesmí objevit výstup z bodů ```2.b, 3``` a ```4``` z atomů jiné molekuly).

### Ukázka výstupů

#### Ukázka č. 1

Spuštění:
```
$ ./h2o 1 0 0 0
```

Výstup (```h2o.out```):

```
1	: H 1	: started
2	: H 1	: waiting
3	: H 2	: started
4	: H 2	: waiting
5	: O 1	: started
6	: O 1	: ready
7	: O 1	: begin bonding
8	: H 2	: begin bonding
9	: H 1	: begin bonding
10	: H 1	: bonded
11	: H 2	: bonded
12	: O 1	: bonded
13	: O 1	: finished
14	: H 2	: finished
15	: H 1	: finished
```

#### Ukázka č. 2

Spuštění:
```
$ ./h2o 3 5 15 10
```

Výstup (```h2o.out```):

```
1	: H 1	: started
2	: H 1	: waiting
3	: H 2	: started
4	: H 2	: waiting
5	: H 3	: started
6	: H 3	: waiting
7	: H 4	: started
8	: H 4	: waiting
9	: H 5	: started
10	: H 5	: waiting
11	: H 6	: started
12	: H 6	: waiting
13	: O 1	: started
14	: O 1	: ready
15	: O 1	: begin bonding
16	: H 2	: begin bonding
17	: H 1	: begin bonding
18	: O 2	: started
19	: O 1	: bonded
20	: H 1	: bonded
21	: H 2	: bonded
22	: O 2	: ready
23	: O 2	: begin bonding
24	: H 3	: begin bonding
25	: H 4	: begin bonding
26	: H 4	: bonded
27	: H 3	: bonded
28	: O 3	: started
29	: O 2	: bonded
30	: O 3	: ready
31	: O 3	: begin bonding
32	: H 5	: begin bonding
33	: H 6	: begin bonding
34	: O 3	: bonded
35	: H 5	: bonded
36	: H 6	: bonded
37	: H 2	: finished
38	: O 1	: finished
39	: H 1	: finished
40	: H 4	: finished
41	: O 2	: finished
42	: H 3	: finished
43	: H 6	: finished
44	: H 5	: finished
45	: O 3	: finished
```

#### Ukázka č. 3 - chybný výstup

Spuštění:
```
$ ./h2o 3 5 15 10
```

Výstup (```h2o.out```):

```
1	: H 1	: started
2	: H 1	: waiting
3	: H 2	: started
4	: H 2	: waiting
5	: H 3	: started
6	: H 3	: waiting
7	: H 4	: started
8	: H 4	: waiting
9	: H 5	: started
10	: H 5	: waiting
11	: H 6	: started
12	: H 6	: waiting
13	: O 1	: started
14	: O 1	: ready
15	: O 1	: begin bonding
16	: H 2	: begin bonding
17	: H 1	: begin bonding
18	: O 2	: started
19	: H 2	: bonded
20	: H 1	: bonded
21	: O 1	: bonded
22	: O 2	: ready
23	: O 2	: begin bonding
24	: H 3	: begin bonding
25	: H 4	: begin bonding
26	: O 3	: started
27	: O 2	: bonded
28	: O 3	: ready
29	: O 3	: begin bonding
30	: H 4	: bonded
31	: H 5	: begin bonding
32	: H 3	: bonded
33	: H 6	: begin bonding
34	: H 6	: bonded
35	: H 5	: bonded
36	: O 3	: bonded
37	: O 2	: finished
38	: H 4	: finished
39	: H 1	: finished
40	: O 1	: finished
41	: H 5	: finished
42	: H 3	: finished
43	: O 3	: finished
44	: H 2	: finished
45	: H 6	: finished
```

**Popis chyby:** řádky ```28``` až ```32```; nebyla dokončena molekula tvořená atomy ```O2```, ```H3``` a ```H4``` a začíná se tvořit molekula tvořená atomy ```O3```, ```H5``` a ```H6``` (na řádcích ```28``` a ```29``` je připravený atom ```O3``` a začíná tvořit novou molekulu; předchozí molekula je však dokončena až na řádku ```32```).

#### Ukázka č. 4 - chybný výstup

Spuštění:
```
$ ./h2o 2 0 0 0
```

Část výstupu (```h2o.out```):

```
1	: H 1	: started
2	: H 1	: waiting
3	: H 2	: started
4	: H 2	: waiting
5	: O 1	: started
6	: O 2	: started
7	: O 1	: ready
8	: O 2	: ready
9	: O 1	: begin bonding
...
```

Popis chyby: řádek ```8``` je v rozporu s body ```2``` a ```6``` popisu procesu. Poslední chybějící atom ```O1``` z molekuly tiskne ready na řádku ```7```. Od tohoto okamžiku začíná proces tvorby molekuly (bod ```2.b.II```); další atomy musí počkat (bod ```2.a```) než se úplně dokončí molekula (bod ```6```).

### Podmínky vypracování projektu

#### Obecné informace

* Projekt implementujte v jazyce C.
* Komentujte zdrojové kódy, programujte přehledně. Součástí hodnocení bude i kvalita zdrojového kódu.
* Kontrolujte, zda se všechny procesy ukončují korektně a zda při ukončování správně uvolňujete všechny alokované zdroje (např. příkazem ```ipcs``` můžete zjistit, jaké zdroje System V jsou v systému alokovány).
* Dodržujte syntax zadaných jmen, formát souborů a formát výstupních dat! Čtěte pozorně zadání a poznámky k vypracování u jednotlivých zadání.
* Projekt musí být přeložitelný a spustitelný na počítači ```merlin.fit.vutbr.cz```.
* Dotazy k zadání: Veškeré nejasnosti a dotazy řešte pouze prostřednictvím diskuzního fóra k projektu 2.

#### Překlad
* Pro překlad používejte nástroj make. Součástí odevzdání bude soubor Makefile.
* Překlad se provede příkazem make v adresáři, kde je umístěn soubor Makefile.
* Po překladu vznikne spustitelný soubor se jménem h2o, který bude umístěn ve stejném adresáři jako soubor Makefile.
* Zdrojové kódy překládejte s přepínači -std=gnu99 -Wall -Wextra -Werror -pedantic.
* Binární program sestavte s přepínačem -pthread.
