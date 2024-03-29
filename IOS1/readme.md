# IOS - Operační systémy - projekt 1

Cílem úlohy je vytvořit skript (tzv. wrapper), který bude spouštět textový editor. Skript si bude pamatovat, které soubory byly v jakém adresáři prostřednictvím daného skriptu editovány. Pokud bude skript spuštěn bez parametrů, vybere skript soubor, který má být editován.

Specifikace chování skriptu

#### JMÉNO

```wedi - wrapper textového editoru s možností automatického výběru souboru```

#### POUŽITÍ
```bash
wedi [ADRESÁŘ]
wedi -m [ADRESÁŘ]
wedi SOUBOR
wedi -l [ADRESÁŘ]
wedi -b|-a DATUM [ADRESÁŘ]
```

#### POPIS

1. Pokud byl skriptu zadán soubor, bude editován.
2. Pokud není argumentem skriptu zadáno jméno editovaného souboru, skript z daného adresáře vybere soubor pro editaci. Výběr je následující:
	* Pokud bylo v daném adresáři editováno skriptem více souborů, vybere se soubor, který byl pomocí skriptu editován jako poslední. Editací souboru se myslí třeba i prohlížení jeho obsahu pomocí skriptu (tj. není nutné, aby byl soubor změněn).
	* Pokud byl zadán argument -m, vybere se soubor, který byl pomocí skriptu editován nejčastěji.
	* Pokud nebyl v daném adresáři editován ještě žádný soubor, jedná se o chybu.
	* Pokud nebyl zadán adresář, předpokládá se aktuální adresář.
3. Skript dokáže také zobrazit seznam všech souborů (argument -l), které byly v daném adresáři editovány.
4. Pokud byl zadán argument -b resp. -a (before, after), skript zobrazí seznam souborů, které byly editovány před resp. po zadaném datu. DATUM je formátu YYYY-MM-DD. Jsou zobrazeny soubory, které byly skriptem editovány od daného data včetně.

#### NASTAVENÍ A KONFIGURACE

1. Skript si pamatuje informace o svém spouštění v souboru, který je dán proměnnou WEDI_RC. Pokud není proměnná nastavena, jedná se o chybu. Formát souboru není specifikován.
2. Skript spouští editor, který je nastaven v proměnné EDITOR. Pokud není proměnná EDITOR nastavená, respektuje proměnnou VISUAL. Pokud ani ta není nastavená, jedná se o chybu.

#### NÁVRATOVÁ HODNOTA

1. Skript vrací úspěch v případě úspěšné operace nebo v případě úspěšné editace. Pokud editor vrátí chybu, skript vrátí stejný chybový návratový kód. Interní chyba skriptu bude doprovázena chybovým hlášením.

#### POZNÁMKY

1. Skript nebere v potaz soubory, se kterými dříve počítal a které jsou nyní smazané.
2. Při rozhodování relativní cesty adresáře je doporučeno používat reálnou cestu (realpath), např.:

```
$ wedi .
$ wedi `pwd`
```

#### Implementační detaily

* Skript by měl běžet na všech běžných shellech (dash, ksh, bash). Ve školním prostředí můžete použít základní (POSIX) /bin/sh.
* Skript by měl ošetřit i chybový případ, že na daném stroji utilita realpath není dostupná.
* Jako referenční stroj můžete použít merlin (Linux) a eva (FreeBSD).
* Skript nesmí používat dočasné soubory.


#### Příklady použití

Následující příklady předpokládají, že skript wedi je dostupný v jedné z cest v proměnné PATH.

* Editace souboru:

```
$ date
Mon  2 Mar 14:45:24 CET 2015
$ wedi ~/.ssh/config
$ wedi ~/.local/bin/wedi
$ wedi ~/.bashrc
$ wedi ~/.indent.pro
...

$ date
Wed  4 Mar 19:51:02 CET 2015
$ wedi ~/.bashrc
$ wedi ~/.vimrc
```

* Opětovná editace:

```
$ cd ~/.ssh
$ wedi
... spusti se editace souboru ~/.ssh/config
$ wedi ~
... spusti se editace souboru ~/.vimrc
$ cd
$ wedi -m
... spusti se editace souboru ~/.bashrc
```

* Zobrazení seznamu editovaných souborů:

```
$ wedi -l $HOME
.bashrc
.indent.pro
.vimrc
$ wedi -a 2015-03-04 $HOME
.bashrc
.vimrc
$ wedi -b 2015-03-03 $HOME
.bashrc
.indent.pro
```
