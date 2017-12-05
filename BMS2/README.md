# BMS - Bezdrátové a mobilní sítě

### Projekt 2

Naprogramujte v jazyku **C/C++** dva konzolové programy (bms2A – modulátor, bms2B – demodulátor), které budou modulovat vstupnou resp. demodulovat výstupnou postupnost bitů za pomoci modulace QPSK.

Vstupná posloupnost bitů modulátoru bude zadaná v textovém souboru, který může obsahovat jen znaky 0 a 1. Jako přenosové médium analogového signálu bude použit WAV soubor s jedním kanálem, přičemž amplituda nosního signálu bude **0x7f000000**.

Parametre modulace zvolte následující:

Frekvence nosné: `1000 Hz`.

Přenosová rychlost minimálně: `1000 bit/s`.

Na začátek pronášených dat přidejte synchronizační sekvenci pro demodulátor: `00110011`.

Parametre demodulace zvolte následující:

QPSK s frekvenci nosné `1000 Hz.`

Baudovú/přenosovou rychlost určete na základě synchronizační sekvence příchozího signálu.

Výstup demodulátoru tvoří opět textový soubor obsahující znaky **0** a **1** bez synchronizační sekvence.
Poznámky:

Při implementaci použijte knihovnu `libsndfile` na jednoduchou práci s WAV soubory.
Můžete použít připravenou kostru, která generuje analogový signál o frekvenci 1 kHz.

Vaše programy musí být schopné pracovat s referenčními vstupy, které jsou vytvořeny v souladu s definovanými parametry.
Při implementaci je možnost použít libovolnou knihovnu dostupnou na serveru merlin.fit.vutbr.cz, na kterém se budou vaše programy testovat. Práce budou testované na shodu zdrojových kódů, u studentů se shodou > 85% bude vyžadována obhajoba. Při přejímání kódu z internetu, uveďte zdroj alespoň v komentáři zdrojového kódu.

* Odevzdávejte soubor ve formátu **ZIP** (TAR a 7Zip a jiné == 0 bodů)

* Daný archiv je pojmenovaný vaším loginem: `xlogin00.zip`

* Archiv nebude obsahovat soubor **libsndfile.a**, ten bude dodaný při opravě.

* Daný archiv v sobě neobsahuje žádné složky: (žádné složky pojmenované vašim loginem, src, projekt a podobně)

* Makefile vytvoří současně oba programy - `./bms2A a ./bms2B`

* Za běhu programů nevypisujte na stdout zbytečnosti, případné chyby pište na stderr

* Dokumentace: dostatečně okomentovaný zdrojový kód

**Parametry programu A:**
```
./bms2A nazev_souboru.txt
```
Funkce programu A:
Program načte vstupní textový soubor – nazev_souboru.txt, jednotlivé znaky moduluje pomocí QPSK na analogový signál. Výstupem bude soubor - `nazev_souboru.wav`.

**Parametry programu B:**
```
./bms2B nazev_souboru.wav
```
Funkce programu B:
Načte vstupní WAV soubor - nazev_souboru.wav a analogový signál demoduluje pomocí QPSK na původní text. Výstupem bude textový soubor – `nazev_souboru.txt`.
