#!/bin/sh

# @description Testovaci script pre ISA projekt
# @author Peter Tisovcik <xtisov00@fit.vutbr.cz>
# @date 15.11.2016
#
#Funkcia test() pomocou, ktorej sa volaju jednotlive testy
#vyuziva funkcie check() a checkStatus().
#
#Funkcia check() zistuje aky navratovy kod vratil program.
#
#Funkcia checkStatus() kontroluje, ci sa zhoduje navratovy
#kod s predpokladanym korektnym alebo nekorektnym ukoncenim
#programu.

#nastavenie cesty k binarke a kompilacia
BIN="./analyzer"
REF_OUTPUT_DIR="ref_output"
EXIT_CODE=0
PRINT_ERROR_STDOUT=0 #1 pre vypis na stdout

SEPARATOR="============================================================="

#nastavenie farieb pre evu
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
	echo ${SEPARATOR}
}

#vytvori referencne subory z vystupu programu
createTest() {
	eval ${1} > ${REF_OUTPUT_DIR}/${3}
}

#porovna vystupy programu s referencnymi
testcompare() {
	echo "Popis testu: ${CYAN}${4}${RESET}"
	echo "Spustenie: ${CYAN}${1}${RESET}"
	if [ ${PRINT_ERROR_STDOUT} -eq 0 ] ; then
		eval ${1} > tmp 2>/dev/null
	else
		eval ${1} > tmp
	fi
	checkStatus $? $2

	diff "${REF_OUTPUT_DIR}/${3}" tmp > tmp_diff
	RET_VAL=$?
	echo "Diff: ${CYAN}diff \"${REF_OUTPUT_DIR}/${3}\" tmp${RESET}"

	echo -n "Zhoda vystupu s referencnym: "
	if [ ${RET_VAL} -eq 0 ] ; then
		echo "${GREEN}OK${RESET}"
	else
		echo " ${RED}ERR${RESET}"
		echo "${CYAN}Pozadovany vystup: ${RESET}"
		cat "ref_output/${3}"

		echo "${CYAN}Vystup programu: ${RESET}"
		cat tmp
	fi
	echo ${SEPARATOR}
}


#Poznamka:
# - v pripade, ze sa v teste pouziva bodkociarka vramci argumentu parametra,
#   je nutne dat tento argument do uvozdoviek \"nieco;nieco\"
#   linux berie ; ako oddelovac prikazov

#pcap pre testovanie argumentov
ARG_PCAP="pcap/file.pcap"

#Test argumentov
test "${BIN}" $ERR "Program bez parametrov."
test "${BIN} -i ${ARG_PCAP}" $ERR "Nespravny pocet potrebnych parametrov."
test "${BIN} -i ${ARG_PCAP} -f" $ERR "Nespravny pocet potrebnych parametrov."
test "${BIN} -i ${ARG_PCAP} -f ipv4" $ERR "Nespravny pocet potrebnych parametrov."
test "${BIN} -i ${ARG_PCAP} -f ipv4 -v 192.168.1.1 " $ERR "Nespravny pocet potrebnych parametrov."
test "${BIN} -i ${ARG_PCAP} -f ipv4 -v 192.168.1.1 -s -haf" $ERR "Nespravny pocet potrebnych parametrov + neexistujuci parameter."
test "${BIN} -i ${ARG_PCAP} -f ipv4 -f ipv4 -v 192.168.1.1 -s" $ERR "Opakujuci sa parameter filter"
test "${BIN} -i ${ARG_PCAP} -f ipv4 -f ipv4 -v 192.168.1.1 -v 192.168.1.1 -s" $ERR "Opakujuci sa hodnota filtra"
test "${BIN} -i ${ARG_PCAP} -f mac -v ff:ff:ff:ff:ff:ff -s" $OK "Kontrola spravneho vstupu - mac"
test "${BIN} -i ${ARG_PCAP} -f mac -v ff:ff:7d:ff:ff:FF -s" $OK "Kontrola spravneho vstupu - mac"
test "${BIN} -i ${ARG_PCAP} -f mac -v ff:ff:7d:ff:ff -s" $ERR "Kontrola nespravneho vstupu - mac"
test "${BIN} -i ${ARG_PCAP} -f mac -v ff:ff:7d:ff:ff::ff -s" $ERR "Kontrola nespravneho vstupu - mac"
test "${BIN} -i ${ARG_PCAP} -f mac -v ff:ff:7d:ff:ff::fg -s" $ERR "Kontrola nespravneho vstupu - mac"
test "${BIN} -i ${ARG_PCAP} -f ipv4 -v 192.185.1.1 -s" $OK "Kontrola spravneho vstupu - ipv4"
test "${BIN} -i ${ARG_PCAP} -f ipv4 -v 0.0.0.0 -s" $OK "Kontrola spravneho vstupu - ipv4"
test "${BIN} -i ${ARG_PCAP} -f ipv4 -v 192.185.1.1555 -s" $ERR "Kontrola nespravneho vstupu - ipv4"
test "${BIN} -i ${ARG_PCAP} -f tcp -v 789789879 -s" $ERR "Kontrola nespravneho vstupu - tcp port, moc velke cislo"
test "${BIN} -i ${ARG_PCAP} -f tcp -v \"-45\" -s" $ERR "Kontrola nespravneho vstupu - tcp port"
test "${BIN} -i ${ARG_PCAP} -f tcp -v 80s -s" $ERR "Kontrola nespravneho vstupu - tcp port"

test "${BIN} -i ${ARG_PCAP} -f tcp -v top10 -s" $OK "Kontrola top 10"
test "${BIN} -i ${ARG_PCAP} -f mac -v top10 -s" $OK "Kontrola top 10"
test "${BIN} -i ${ARG_PCAP} -f ipv6 -v top10 -s" $OK "Kontrola top 10"
test "${BIN} -i ${ARG_PCAP} -f tcp,udp -v top10 -s" $ERR "Kontrola top 10 - moze mat len jeden filter"
test "${BIN} -i ${ARG_PCAP} -f mac,ipv6 -v top10 -s" $ERR "Kontrola top 10 - moze mat len jeden filter"

test "${BIN} -i ${ARG_PCAP} -f tcp -v top10 " $ERR "Kontrola top 10 - musi mat source/destination"
test "${BIN} -i ${ARG_PCAP} -f tcp -v top10 -s" $OK "Kontrola top 10 - musi mat source/destination"
test "${BIN} -i ${ARG_PCAP} -f tcp -v top10 -d" $OK "Kontrola top 10 - musi mat source/destination"
test "${BIN} -i ${ARG_PCAP} -f tcp -v top10 -d -s" $OK "Kontrola top 10 - musi mat source/destination"
test "${BIN} -i ${ARG_PCAP} -f tcp -v top10 -d -s" $OK "Kontrola top 10 - musi mat source/destination"
test "${BIN} -i ${ARG_PCAP} -f tcp, -v top10 -d -s" $ERR "Kontrola top 10 - neplatny vstup"

test "${BIN} -i ${ARG_PCAP} -f tcp, -v 80 -d -s" $ERR "Kontrola parametrov v rozsireni - zadana ciarka"
test "${BIN} -i ${ARG_PCAP} -f tcp,udp -v 80 -d -s" $ERR "Kontrola parametrov v rozsireni - nezadany port"
test "${BIN} -i ${ARG_PCAP} -f tcp,tcp -v \"80;80\" -d -s" $ERR "Kontrola parametrov v rozsireni - opakuje sa tcp"
test "${BIN} -i ${ARG_PCAP} -f ipv4,ipv4 -v \"192.168.1.1;192.168.1.1\" -d -s" $ERR "Kontrola parametrov v rozsireni - opakuje sa ipv4 filter"

test "${BIN} -i ${ARG_PCAP} -f tcp\; -v 80 -d -s" $ERR "Kontrola parametrov v rozsireni - zadana ciarka"
test "${BIN} -i ${ARG_PCAP} -f tcp\;udp -v 80 -d -s" $ERR "Kontrola parametrov v rozsireni - nezadany port"
test "${BIN} -i ${ARG_PCAP} -f tcp\;tcp -v \"80;80\" -d -s" $ERR "Kontrola parametrov v rozsireni - opakuje sa tcp"
test "${BIN} -i ${ARG_PCAP} -f ipv4\;ipv4 -v \"192.168.1.1;192.168.1.1\" -d -s" $ERR "Kontrola parametrov v rozsireni - opakuje sa ipv4 filter"

test "${BIN} -i ${ARG_PCAP} -f tcp\;ipv4 -v \"80;192.168.1.1\" -d -s" $OK "Kontrola parametrov v rozsireni - opakuje sa tcp"
test "${BIN} -i ${ARG_PCAP} -f tcp,ipv4 -v \"192.168.1.1;80\" -d -s" $ERR "Kontrola parametrov v rozsireni - vymenene poradie hodnot filtra"

test "${BIN} -i ${ARG_PCAP} -f mac -v 5C:D5:96:2C:38:63" $ERR "Chybny pocet argumentu (mene)."
test "${BIN} -i ${ARG_PCAP} -f ipv4 -v 5C:D5:96:2C:38:63 -d -s -r" $ERR "Chybny pocet argumentu (vice)."
test "${BIN} -j ${ARG_PCAP} -f ipv6 -v 5C:D5:96:2C:38:63 -d" $ERR "Chybny prepinac."
test "${BIN} -i ${ARG_PCAP} -g tcp -v 5C:D5:96:2C:38:63 -s" $ERR "Chybny prepinac."
test "${BIN} -i ${ARG_PCAP} -f udp -w 5C:D5:96:2C:38:63 -d" $ERR "Chybny prepinac."
test "${BIN} -i ${ARG_PCAP} -f mac -v 5C:D5:96:2C:38:63 -t" $ERR "Chybny prepinac."
test "${BIN} -i -f mac -v 5C:D5:96:2C:38:63 -s" $ERR "Chybi nazev souboru."
test "${BIN} -i ${ARG_PCAP} -f -v 5C:D5:96:2C:38:63 -d" $ERR "chybi typ filteru."
test "${BIN} -i ${ARG_PCAP} -f mac -v -s" $ERR "chybi hodnota filteru."
test "${BIN} -i ${ARG_PCAP} -f max -v 5C:D5:96:2C:38:63 -d -f mac -v -s" $ERR "chybny typ filteru."


#Test pocitania packetov
testcompare "${BIN} -i pcap/isa.pcap -f udp -v 101,104 -s" $OK "holkovic01" ""
testcompare "${BIN} -i pcap/isa.pcap -f tcp -v 101 -s -d" $OK "holkovic02" ""
testcompare "${BIN} -i pcap/isa.pcap -f tcp -v 103 -s" $OK "holkovic03" ""
testcompare "${BIN} -i pcap/isa.pcap -f ipv4 -v 10.10.10.60 -d" $OK "holkovic04" ""
testcompare "${BIN} -i pcap/isa.pcap -f ipv4 -v 10.10.10.100 -s -d" $OK "holkovic05" ""
testcompare "${BIN} -i pcap/isa.pcap -f mac -v 00:00:00:00:00:05 -s" $OK "holkovic06" ""

testcompare "${BIN} -i pcap/isa.pcap -f ipv4 -v top10 -s" $OK "martin01" ""
testcompare "${BIN} -i pcap/isa.pcap -f ipv6 -v top10 -s" $OK "martin02" ""
testcompare "${BIN} -i pcap/isa.pcap -f tcp -v top10 -s" $OK "martin03" ""
testcompare "${BIN} -i pcap/isa.pcap -f udp -v top10 -s" $OK "martin04" ""
testcompare "${BIN} -i pcap/isa.pcap -f ipv4\;tcp -v \"10.10.10.6;100\" -s" $OK "martin05" ""
testcompare "${BIN} -i pcap/IPv6inIPv4.pcap -f ipv4 -v top10 -s" $OK "martin06" ""
testcompare "${BIN} -i pcap/IPv6inIPv4.pcap -f ipv6 -v top10 -s" $OK "martin07" ""

testcompare "${BIN} -i pcap/isa.pcap -f mac -v top10 -d" $OK "peto01" ""
testcompare "${BIN} -i pcap/isa.pcap -f mac -v top10 -d -s" $OK "peto02" ""
testcompare "${BIN} -i pcap/isa.pcap -f ipv4 -v top10 -d" $OK "peto03" ""
testcompare "${BIN} -i pcap/isa.pcap -f ipv4 -v top10 -d -s" $OK "peto04" ""
testcompare "${BIN} -i pcap/isa.pcap -f ipv6 -v top10 -d" $OK "peto05" ""
testcompare "${BIN} -i pcap/isa.pcap -f ipv6 -v top10 -d -s" $OK "peto06" ""
testcompare "${BIN} -i pcap/isa.pcap -f tcp -v top10 -d" $OK "peto07" ""
testcompare "${BIN} -i pcap/isa.pcap -f tcp -v top10 -d -s" $OK "peto08" ""
testcompare "${BIN} -i pcap/isa.pcap -f udp -v top10 -d" $OK "peto09" ""
testcompare "${BIN} -i pcap/isa.pcap -f udp -v top10 -d -s" $OK "peto10" ""

testcompare "${BIN} -i pcap/petoTraffic.pcap -f mac -v top10 -d" $OK "peto11" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f mac -v top10 -s" $OK "peto12" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f mac -v top10 -d -s" $OK "peto13" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f ipv4 -v top10 -d" $OK "peto14" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f ipv4 -v top10 -s" $OK "peto15" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f ipv4 -v top10 -d -s" $OK "peto16" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f ipv6 -v top10 -d" $OK "peto17" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f ipv6 -v top10 -s" $OK "peto18" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f ipv6 -v top10 -d -s" $OK "peto19" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f tcp -v top10 -d" $OK "peto20" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f tcp -v top10 -s" $OK "peto21" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f tcp -v top10 -d -s" $OK "peto22" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f udp -v top10 -d" $OK "peto23" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f udp -v top10 -s" $OK "peto24" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f udp -v top10 -d -s" $OK "peto25" ""

testcompare "${BIN} -i pcap/petoTraffic.pcap -f mac -v e8:de:27:47:8f:94 -s" $OK "peto26" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f mac -v e8:de:27:47:8f:94 -d" $OK "peto27" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f mac -v e8:de:27:47:8f:94 -s -d" $OK "peto28" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f mac -v 1c:1b:0d:04:7d:50 -s" $OK "peto29" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f mac -v 1c:1b:0d:04:7d:50 -d" $OK "peto30" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f mac -v 1c:1b:0d:04:7d:50 -s -d" $OK "peto31" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f ipv4 -v 192.168.0.1 -s" $OK "peto32" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f ipv4 -v 192.168.0.1 -d" $OK "peto33" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f ipv4 -v 192.168.0.1 -s -d" $OK "peto34" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f tcp -v 36078 -s" $OK "peto35" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f tcp -v 36078 -d" $OK "peto36" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f tcp -v 36078 -s -d" $OK "peto37" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f tcp -v 60766 -s" $OK "peto38" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f tcp -v 60766 -d" $OK "peto39" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f tcp -v 60766 -s -d" $OK "peto40" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f tcp -v 22 -s" $OK "peto41" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f tcp -v 22 -d" $OK "peto42" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f tcp -v 22 -s -d" $OK "peto43" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f tcp -v 80 -s" $OK "peto44" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f tcp -v 80 -d" $OK "peto45" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f tcp -v 80 -s -d" $OK "peto46" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f udp -v 3300 -s" $OK "peto47" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f udp -v 3300 -d" $OK "peto48" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f udp -v 3300 -s -d" $OK "peto49" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f udp -v 53 -s" $OK "peto50" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f udp -v 53 -d" $OK "peto51" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f udp -v 53 -s -d" $OK "peto52" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f udp -v 5353 -s" $OK "peto53" "rovnake porty src-dst"
testcompare "${BIN} -i pcap/petoTraffic.pcap -f udp -v 5353 -d" $OK "peto54" "rovnake porty src-dst"
testcompare "${BIN} -i pcap/petoTraffic.pcap -f udp -v 5353 -s -d" $OK "peto55" "rovnake porty src-dst"

testcompare "${BIN} -i pcap/vlans.pcap -f mac -v 00:1c:58:23:64:c1 -d" $OK "klara01" ""
testcompare "${BIN} -i pcap/vlans.pcap -f mac -v 00:1c:58:23:64:c1 -s" $OK "klara02" ""
testcompare "${BIN} -i pcap/vlans.pcap -f mac -v 00:1c:58:23:64:c1 -d -s" $OK "klara03" ""
testcompare "${BIN} -i pcap/vlans.pcap -f mac -v 00:15:62:64:33:41 -d -s" $OK "klara04" ""
testcompare "${BIN} -i pcap/arp.pcap -f mac -v  00:07:0d:af:f4:54 -s" $OK "klara05" ""
testcompare "${BIN} -i pcap/vlans.pcap -f ipv4 -v 20.20.20.2 -s" $OK "klara06" ""
testcompare "${BIN} -i pcap/vlans.pcap -f ipv4 -v 10.10.10.2 -d" $OK "klara07" ""
testcompare "${BIN} -i pcap/vlans.pcap -f ipv4 -v 20.20.20.2 -d" $OK "klara08" ""
testcompare "${BIN} -i pcap/vlans.pcap -f ipv4 -v 20.20.20.2 -d -s" $OK "klara09" ""
testcompare "${BIN} -i pcap/general.pcap -f mac -v 00:12:79:80:69:60 -d" $OK "klara10" ""
testcompare "${BIN} -i pcap/general.pcap -f tcp -v 631 -s" $OK "klara11" ""
testcompare "${BIN} -i pcap/general.pcap -f tcp -v 55342 -s" $OK "klara12" ""
testcompare "${BIN} -i pcap/general.pcap -f tcp -v 55342 -s -d" $OK "klara13" ""

testcompare "${BIN} -i pcap/isa.pcap -f mac -v top10 -s" $OK "radim01" ""
testcompare "${BIN} -i pcap/isa.pcap -f mac -v 00:00:00:00:00:01 -s" $OK "radim02" "RARP"
testcompare "${BIN} -i pcap/isa.pcap -f mac -v 00:00:00:00:00:04 -s" $OK "radim03" "RARP"

testcompare "${BIN} -i pcap/petoTraffic.pcap -f mac\;udp -v ff:ff:ff:ff:ff:ff\;7437 -d" $OK "martin08" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f udp\;ipv4 -v 50119\;192.168.0.1 -s" $OK "martin09" ""
testcompare "${BIN} -i pcap/petoTraffic.pcap -f ipv4\;tcp -v 116.31.116.6,78.11.101.190\;36078,42238 -s" $OK "martin10" ""
