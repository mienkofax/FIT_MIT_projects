#!/bin/sh

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
ERR=1
OK=0

EXIT_CODE=0

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
	${1}
	checkStatus $? $2
}

#Kompilacia suborov
echo "${CYAN}Kompilacia suborov:${RESET}"
make
echo ""

test "./client" $ERR "Program bez parametrov."


















echo ""
