#!/bin/sh

# @description Testovaci script pre server <-> client
# @author Peter Tisovcik <xtisov00@fit.vutbr.cz>
# @date 18.04.2016
#
#Script na testovanie komunikacie medzi serverom a klientom.
#Funkcia test() pomocou, ktorej sa volaju jednotlive testy
#vyuziva funkcie check() a checkStatus().
#
#Funkcia check() zistuje aky navratovy kod vratil program.
#
#Funkcia checkStatus() kontroluje, ci sa zhoduje navratovy
#kod s predpokladanym korektnym alebo nekorektnym ukoncenim
#programu.

EXIT_CODE=0
HOST=`hostname`
if [ "$HOST" = "eva.fit.vutbr.cz" ] ; then
	RED=`echo -e "\033[33;31m"`
	GREEN=`echo -e "\033[33;92m"`
	CYAN=`echo -e "\033[33;36m"`
	RESET=`echo -e "\033[33;39m"`
else
	RED=`echo "\033[33;31m"`
	GREEN=`echo "\033[33;92m"`
	CYAN=`echo "\033[33;36m"`
	RESET=`echo "\033[33;39m"`
fi

#nekorektne ukoncenie programu
ERR=1

#korektne ukoncenie programu
OK=0

#kontrola exit kodu, ci je spravny alebo nie
# $1 - navratovy kod
check() {
	if [ "$1" = "$OK" ] ; then
		echo "${GREEN}OK${RESET}"
		return $OK
	else
		echo "${RED}ERR${RESET}"
		return $ERR
	fi
}

#Kontrola, ci bolo pozadovane ukonceie bud s chybne alebo spravne
# $1 - ocakavane ukoncenie programu
# $2 - ukoncovaci stav programu
checkStatus() {
	echo -n "Ukoncovaci stav programu: "
	check $1
	EXIT_CODE=$?
	echo -n "Ocakavane ukoncenie: "
	check $2
	if [ $? -eq $EXIT_CODE ] ; then
		echo "Status: ${GREEN}OK${RESET}"
	else
		echo "Status: ${RED}ERR - ${1} ${RESET}"
	fi
}

#Funkcia na otestovanie spravnosti
# $1 - prikaz, ktory sa ma vykonat
# $2 - navratovy kod, ktory sa ocakava
# $3 - popis testu
test() {
	echo "Popis testu: ${CYAN}${3}${RESET}"
	echo "Spustenie: ${CYAN}${1}${RESET}"
	eval ${1}
	checkStatus $? $2
	echo "---------------------------------------------------"
}

#Odstranenie nepotrebnych suborov
make clean

#Kompilacia suborov
echo "${CYAN}Kompilacia suborov:${RESET}"
./compile.sh
echo ""

#Poznamka:
# - v pripade, ze sa v teste pouziva bodkociarka vramci argumentu parametra,
#   je nutne dat tento argument do uvozdoviek \"nieco;nieco\"
#   linux berie ; ako oddelovac prikazov

#Test argumentov
#Peto
test "./analyzer" $ERR "Program bez parametrov."
test "./analyzer -i file" $ERR "Nespravny pocet potrebnych parametrov."
test "./analyzer -i file -f" $ERR "Nespravny pocet potrebnych parametrov."
test "./analyzer -i file -f ipv4" $ERR "Nespravny pocet potrebnych parametrov."
test "./analyzer -i file -f ipv4 -v 192.168.1.1 " $ERR "Nespravny pocet potrebnych parametrov."
test "./analyzer -i file -f ipv4 -v 192.168.1.1 -s -haf" $ERR "Nespravny pocet potrebnych parametrov + neexistujuci parameter."
test "./analyzer -i file.pcap -f ipv4 -f ipv4 -v 192.168.1.1 -s" $ERR "Opakujuci sa parameter filter"
test "./analyzer -i file.pcap -f ipv4 -f ipv4 -v 192.168.1.1 -v 192.168.1.1 -s" $ERR "Opakujuci sa hodnota filtra"
test "./analyzer -i file.pcap -f mac -v ff:ff:ff:ff:ff:ff -s" $OK "Kontrola spravneho vstupu - mac"
test "./analyzer -i file.pcap -f mac -v ff:ff:7d:ff:ff:FF -s" $OK "Kontrola spravneho vstupu - mac"
test "./analyzer -i file.pcap -f mac -v ff:ff:7d:ff:ff -s" $ERR "Kontrola nespravneho vstupu - mac"
test "./analyzer -i file.pcap -f mac -v ff:ff:7d:ff:ff::ff -s" $ERR "Kontrola nespravneho vstupu - mac"
test "./analyzer -i file.pcap -f mac -v ff:ff:7d:ff:ff::fg -s" $ERR "Kontrola nespravneho vstupu - mac"
test "./analyzer -i file.pcap -f ipv4 -v 192.185.1.1 -s" $OK "Kontrola spravneho vstupu - ipv4"
test "./analyzer -i file.pcap -f ipv4 -v 0.0.0.0 -s" $OK "Kontrola spravneho vstupu - ipv4"
test "./analyzer -i file.pcap -f ipv4 -v 192.185.1.1555 -s" $ERR "Kontrola nespravneho vstupu - ipv4"
test "./analyzer -i file.pcap -f tcp -v 789789879 -s" $ERR "Kontrola nespravneho vstupu - tcp port, moc velke cislo"
test "./analyzer -i file.pcap -f tcp -v \"-45\" -s" $ERR "Kontrola nespravneho vstupu - tcp port"
test "./analyzer -i file.pcap -f tcp -v 80s -s" $ERR "Kontrola nespravneho vstupu - tcp port"

test "./analyzer -i file.pcap -f tcp -v top10 -s" $OK "Kontrola top 10"
test "./analyzer -i file.pcap -f mac -v top10 -s" $OK "Kontrola top 10"
test "./analyzer -i file.pcap -f ipv6 -v top10 -s" $OK "Kontrola top 10"
test "./analyzer -i file.pcap -f tcp,udp -v top10 -s" $ERR "Kontrola top 10 - moze mat len jeden filter"
test "./analyzer -i file.pcap -f mac,ipv6 -v top10 -s" $ERR "Kontrola top 10 - moze mat len jeden filter"

test "./analyzer -i file.pcap -f tcp -v top10 " $ERR "Kontrola top 10 - musi mat source/destination"
test "./analyzer -i file.pcap -f tcp -v top10 -s" $OK "Kontrola top 10 - musi mat source/destination"
test "./analyzer -i file.pcap -f tcp -v top10 -d" $OK "Kontrola top 10 - musi mat source/destination"
test "./analyzer -i file.pcap -f tcp -v top10 -d -s" $OK "Kontrola top 10 - musi mat source/destination"
test "./analyzer -i file.pcap -f tcp -v top10 -d -s" $OK "Kontrola top 10 - musi mat source/destination"
test "./analyzer -i file.pcap -f tcp, -v top10 -d -s" $ERR "Kontrola top 10 - neplatny vstup"

test "./analyzer -i file.pcap -f tcp, -v 80 -d -s" $ERR "Kontrola parametrov v rozsireni - zadana ciarka"
test "./analyzer -i file.pcap -f tcp,udp -v 80 -d -s" $ERR "Kontrola parametrov v rozsireni - nezadany port"
test "./analyzer -i file.pcap -f tcp,tcp -v \"80;80\" -d -s" $ERR "Kontrola parametrov v rozsireni - opakuje sa tcp"
test "./analyzer -i file.pcap -f tcp,ipv4 -v \"80;192.168.1.1\" -d -s" $OK "Kontrola parametrov v rozsireni - opakuje sa tcp"
test "./analyzer -i file.pcap -f tcp,ipv4 -v \"192.168.1.1;80\" -d -s" $ERR "Kontrola parametrov v rozsireni - vymenene poradie hodnot filtra"





#Klarka
test "./analyzer -i test1.pcap -f mac -v 5C:D5:96:2C:38:63" $ERR "Chybny pocet argumentu (mene)."
test "./analyzer -i test1.pcap -f ipv4 -v 5C:D5:96:2C:38:63 -d -s -r" $ERR "Chybny pocet argumentu (vice)."
test "./analyzer -j test1.pcap -f ipv6 -v 5C:D5:96:2C:38:63 -d" $ERR "Chybny prepinac."
test "./analyzer ./analyzer -i test1.pcap -g tcp -v 5C:D5:96:2C:38:63 -s" $ERR "Chybny prepinac."
test "./analyzer ./analyzer -i test1.pcap -f udp -w 5C:D5:96:2C:38:63 -d" $ERR "Chybny prepinac."
test "./analyzer ./analyzer -i test1.pcap -f mac -v 5C:D5:96:2C:38:63 -t" $ERR "Chybny prepinac."
test "./analyzer ./analyzer -i -f mac -v 5C:D5:96:2C:38:63 -s" $ERR "Chybi nazev souboru."
test "./analyzer ./analyzer -i test1.pcap -f -v 5C:D5:96:2C:38:63 -d" $ERR "chybi typ filteru."
test "./analyzer ./analyzer -i test1.pcap -f mac -v -s" $ERR "chybi hodnota filteru."
test "./analyzer -i test1.pcap -f max -v 5C:D5:96:2C:38:63 -d -f mac -v -s" $ERR "chybny typ filteru."

