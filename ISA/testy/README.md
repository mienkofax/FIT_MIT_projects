# ISA-test
Testy k projektu z predmetu ISA.

# Autori

 * Peter Tisovčík
 * Martin Perešíni

# Popis
Jednotlive testy kontroluju spravnost programu na zaklade zadanych parametrov. Kazdy test obsahuje binarku, ktora sa ma spustit argumenty, ocakavanu navratovu hodnotu programu a popis testu. Bohuzial nie su presne definovane navratove hodnoty takze, hodnoty navratovych kodov je potreba overit skontrolovanim vypisu jednotlivych testov. Pre test parametrov je predpripraveny jednoduchy pcap subor pcap/file.pcap, jeho nazov sa da vlozit do testu premennou ${ARG_PCAP}. Pre testy spravneho pocitania je lepsie zvolit nazov suboru a neuklad pcap subory do premennych. Testy podporuju farebne zobrazenie. Viac informacii sa dozviete zo suboru obsahujuci testy: test.sh.

Pridavanie testov:
------------------
Pre pridavanie testov sa inspirujte existujucimi testami. Pre pridavanie testov, ktore budu obsahovat pcap subory ich umiestnite do adresara pcap. Referencne vystupy je potrebne umiestnit do adresara ref_output. Pre ulahcenie generovania vystupnych suborov je pridana funkcia createTest, ktora ma rovnake parametre ako funkcia comparetest a tym padom staci len vlozit rovnaky prikaz ako pre pridanie testu len s nazvom createTest. Nasledne sa vygeneruju pozadovane subory a je nutne zmenit nazov funkcie z createTest na compareTest, aby bolo mozne vygenerovane tety porovnavat.

Spustenie:
----------
stiahnutie gitu a pridanie opraneni pre spustenie chmod +x test.sh, je nutne mat binarku s analyzatorom v rovnakom adresari ako je test.sh, pripadne je nutne upravit premennu BIN(riadok 17). Premena BIN obsahuje cestu k binarke.

Pospis testu:
------------
test "${BIN} argumenty" navratova hodnota "Popis testu."

testcompare "${BIN} argumenty" $OK "subor" "Popis testu."

Popis konkretneho testu:
---------------------
test "${BIN} -i ${ARG_PCAP} -f ipv4 -v 192.168.1.1 " $ERR "Nespravny pocet potrebnych parametrov."

test "${BIN} -i ${ARG_PCAP} -f ipv4 -v 192.185.1.1 -s" $OK "Kontrola spravneho vstupu - ipv4"

testcompare "${BIN} -i pcap/isa.pcap -f udp -v 101,104 -s" $OK "holkovic01" ""

Popis vystupu jednotlivych vstupov:
----------------------------------
1. riadok: popis testu, definovany v teste
2. riadok: prikaz ktory sa spusta vramci testu + argumenty, definovany v teste
3. riadok: Stav programu s akym skoncil bude OK(return 0) alebo ERR(ostatne chybove kody)
4. riadok: Stav programu ktory sa ocakava, definovany v teste
5. riadok: Kontrola ci program skoncil s pozadovanym navratovym kodom. Ak ano OK, ak nie ERR.
6. riadok: Len pri teste statistik, ak sa zhoduje vystup programu s referencnym vystupom vypise sa OK, inak sa vypise vystup programu a vystup ktory je spravny.

Popis testu: chybny typ filteru. 
Spustenie: ./analyzer -i pcap/file.pcap -f max -v 5C:D5:96:2C:38:63 -d -f mac -v -s
Repeat parameter f
Ukoncovaci stav programu: ERR
Ocakavane ukoncenie: ERR
Status: OK



