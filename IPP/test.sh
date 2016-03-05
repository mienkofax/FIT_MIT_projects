#!/bin/bash

INTERPRET=php
FLAGS='-d open_basedir=""'
FILE=cls.php
ERROR=0
SEPARATOR="-------------------------------------------"

MERLIN_TEST=0
HOSTNAME="merlin.fit.vutbr.cz"
ACT_HOSTNAME=hostname
if [ "$HOSTNAME" == "$ACT_HOSTNAME" ] ; then
	MERLIN_TEST=1
fi


INPUT_FILE=input.test
OUTPUT_FILE=output.test
TMP_FILE=tmp.f

touch $INPUT_FILE
touch $OUTPUT_FILE

echoTest() {
	echo -n -e "\e[36m"
	echo "$1"
	echo -n -e "\e[39m"
}

check () {
	ERR=$?
	OK=0

	#zistenie ci je vysledok programu Ok/ERR
	if [ $ERR -eq 0 ] ; then
		OK=1
	fi

	#zelenou sa potvrdi spravny exit code
	#cercenou je chybny navratovy kod
	if [ "$1" == "OK"  ] && [  $OK -eq 1 ] ; then
		echo -ne "\e[92m"
	elif  [ "$1" == "ERR"  ] && [  $OK -eq 0 ] ; then
		echo -ne "\e[92m"
	else
		echo -ne "\e[31m"
		ERROR=$((ERROR + 1)) #pocet chyb
	fi

	#vypis vysledku
	if [ $ERR -eq 0 ] ; then
		echo "OK"
	else
		echo "ERR: $ERR"
	fi

	#reset farby
	echo -n -e "\e[39m"
}

echo $SEPARATOR

echoTest "TEST: 01/OK - zobrazenie help spravy"
$INTERPRET $FLAGS $FILE --help
check "OK"
echo $SEPARATOR

echoTest "TEST: 02/ERR - opakovanie help prepinaca "
$INTERPRET $FLAGS $FILE --help --help
check "ERR"
echo $SEPARATOR

echoTest "TEST: 03/OK - prepinac input s platnym nazvom suboru"
$INTERPRET $FLAGS $FILE --input=$INPUT_FILE
check "OK"
echo $SEPARATOR

echoTest "TEST: 04/ERR - prepinac input s neplatnym nazvom suboru"
$INTERPRET $FLAGS $FILE --input=asdfhjsdfsdafsafds
check "ERR"
echo $SEPARATOR

if [ $MERLIN_TEST -eq 0 ] ; then
echoTest "TEST: 05/ERR - prepinac input s platnym suborom ale nedostatocnym opravnenim na citanie"
sudo touch $TMP_FILE
sudo chmod o-r $TMP_FILE
$INTERPRET $FLAGS $FILE --input=$TMP_FILE
check "ERR"
echo $SEPARATOR
sudo rm $TMP_FILE
fi

echoTest "TEST: 06/ERR - nezmami prepinac "
$INTERPRET $FLAGS $FILE --unknown
check "ERR"
echo $SEPARATOR

echoTest "TEST: 07/OK - prepinac output s platnym nazvom suboru"
$INTERPRET $FLAGS $FILE --output=$INPUT_FILE
check "OK"
echo $SEPARATOR

if [ $MERLIN_TEST -eq 0 ] ; then
echoTest "TEST: 08/ERR - prepinac output s platnym suborom ale nedostatocnym opravnenim na zapis"
sudo touch $TMP_FILE
sudo chmod o-r $TMP_FILE
$INTERPRET $FLAGS $FILE --output=$TMP_FILE
check "ERR"
echo $SEPARATOR
sudo rm $TMP_FILE
fi

echoTest "TEST: 09/OK - prepinac pretty-xml s platnym cislom"
$INTERPRET $FLAGS $FILE --pretty-xml=5
check "OK"
echo $SEPARATOR

echoTest "TEST: 10/ERR - prepinac pretty-xml so zapornym cislom"
$INTERPRET $FLAGS $FILE --pretty-xml=-5
check "ERR"
echo $SEPARATOR

echoTest "TEST: 11/ERR - prepinac pretty-xml s retazcom"
$INTERPRET $FLAGS $FILE --pretty-xml=number
check "ERR"
echo $SEPARATOR

echoTest "TEST: 12/ERR - prepinac pretty-xml s opakovanym prepinacom"
$INTERPRET $FLAGS $FILE --pretty-xml=5 --pretty-xml=5
check "ERR"
echo $SEPARATOR

echoTest "TEST: 13/OK - prepinac pretty-xml bez parametra"
$INTERPRET $FLAGS $FILE --pretty-xml
check "OK"
echo $SEPARATOR

echoTest "TEST: 14/ERR - duplikovanie kratkeho a dlheho prepinaca - output"
$INTERPRET $FLAGS $FILE -o=file --output=file
check "ERR"
echo $SEPARATOR

echoTest "TEST: 15/ERR - help s inymi prepinacmi"
$INTERPRET $FLAGS $FILE -o=file  --help
check "ERR"
echo $SEPARATOR

echoTest "TEST: 16/OK - kratky prepinac -o"
$INTERPRET $FLAGS $FILE -o=file
check "OK"
echo $SEPARATOR

echoTest "TEST: 17/OK - kratky prepinac -h"
$INTERPRET $FLAGS $FILE -h
check "OK"
echo $SEPARATOR

echoTest "TEST: 18/OK - kratky prepinac -p"
$INTERPRET $FLAGS $FILE -o=file
check "OK"
echo $SEPARATOR

echoTest "TEST: 19/ERR - kratky prepinac -hxxxx"
$INTERPRET $FLAGS $FILE -hxxxx
check "ERR"
echo $SEPARATOR

echoTest "TEST: 20/ERR - dlhy prepinac input bez ="
$INTERPRET $FLAGS $FILE --output $OUTPUT_FILE
check "ERR"
echo $SEPARATOR



################################
if [ $ERROR -eq 0 ] ; then
	echo -ne "\e[92m"
else
	echo -ne "\e[31m"
fi

#pocet chyb
echo "POCET CHYB: $ERROR"
echo -n -e "\e[39m" #reset farby

echo $SEPARATOR

rm $INPUT_FILE
rm $OUTPUT_FILE
