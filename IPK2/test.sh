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

#port na, ktorom sa spusta server
PORT=15346

#kontrola exit kodu, ci je je spravne alebo Najdenie
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
	echo -n "Ocakavane ukoncenie: "
	check $1
	EXIT_CODE=$?
	echo -n "Ukoncovaci stav programu:  "
	check $2
	if [ $? -eq $EXIT_CODE ] ; then
		echo "Status: ${GREEN}OK${RESET}"
	else
		echo "Status: ${GREEN}ERR${RESET}"
	fi
}

#Funkcia na otestovanie spravnosti
# $1 - prikaz, ktory sa ma vykonat
# $2 - navratovy kod, ktory sa ocakava
# $3 - popis testu
test() {
	echo "Popis testu: ${CYAN}${3}${RESET}"
	echo "Spustenie: ${CYAN}${1}${RESET}"
	${1}
	checkStatus $? $2
	echo ""
}

#Kompilacia suborov
echo "${CYAN}Kompilacia suborov:${RESET}"
make
echo ""

#Test argumentov
test "./client" $ERR "Program bez parametrov."
test "./client -unknown" $ERR "Program bez parametrov."
test "./client -h" $ERR "Program s neuplnymi argumentami."
test "./client -h hostname" $ERR "Program s neuplnymi argumentami."
test "./client -h hostname -p " $ERR "Program s neuplnymi argumentami."
test "./client -h hostname -p 8008" $ERR "Program s neuplnymi argumentami."
test "./client -h hostname -p 8008" $ERR "Program s neuplnymi argumentami."
test "./client -h hostname -p 8008 -d" $ERR "Program s neuplnymi argumentami."
test "./client -h hostname -p 8008 -u" $ERR "Program s neuplnymi argumentami."
test "./client -h hostname -p 8008 -u Makefile -u Makefile" $ERR "Program s uplnymi parametrami ale s duplicitnum prepinacom -u."
test "./client -h hostname -p -1 -d Makefile" $ERR "Program s uplnymi argumentami ale chybnymi parametrami(zle cislo portu)."
test "./client -h hostname -p -100000 -d Makefile" $ERR "Program s uplnymi argumentami ale chybnymi parametrami(zle cislo portu)."

test "./server" $ERR "Program bez parametrov."
test "./server -unknown" $ERR "Program bez parametrov."
test "./server -p" $ERR "Program s neuplnymi argumentami."
test "./server -p -1" $ERR test "./client -h hostname -p -1 -d Makefile" $ERR "Program s uplnymi argumentami ale chybnymi parametrami(zle cislo portu)."
test "./server -p 100000" $ERR test "./client -h hostname -p -1 -d Makefile" $ERR "Program s uplnymi argumentami ale chybnymi parametrami(zle cislo portu)."

#Test funkcnosti chyb v programe
test "./client -p 22 -h localhost -u Makefile" $ERR "Program s rezervovanym cislom portu."
test "./client -p 8008 -h hostname -u Makefile" $ERR "Program s neznamim hostname."
test "./server -p 80" $ERR "Program s pouzivanym cislom portu."

#Spustenie servera
./server -p ${PORT} &
SERVER_PID=$!
sleep 1

#Test stiahnutia/odoslania suboru v rovnakom adresari
test "./client -p ${PORT} -h localhost -u Makefile" $OK "Stiahnutie suboru v rovnakom adresari."
test "./client -p ${PORT} -h localhost -d Makefile" $OK "Odoslanie suboru v rovnakom adresari."

#Test stiahnutia/odoslania suboru v inom adresari
mkdir testDir
cp client testDir

#vytvorenie suboru
truncate -s 10M test.file
test "./testDir/client -p ${PORT} -h localhost -d Makefile" $OK "Stiahnutie suboru v rovnakom adresari."
test "./testDir/client -p ${PORT} -h localhost -u test.file" $OK "Odoslanie suboru v rovnakom adresari."


#Neexistujuci subor
test "./client -p ${PORT} -h localhost -d neexistujuci" $ERR "Poziadavka na stiahnutie neexistujuceho suboru."
test "./client -p ${PORT} -h localhost -d neexistujuci" $ERR "Poziadavka na odoslanie neexistujuceho suboru."

sleep 1
kill $SERVER_PID
