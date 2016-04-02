#!/bin/bash

INTERPRET=python3
FILE=csv.py

JEXAMXML_INTERPRET="java -jar jexamxml.jar"
JEXAMXML_OPTIONS=csv_options
JEXAMXML_INPUT=output.xml
JEXAMXML_DELTA=delta.xml

TEST_DIR="testy"
REF_INPUT_DIR=$TEST_DIR
REF_OUTPUT_DIR=$TEST_DIR"/ref-out"
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

##################################
# Test argumentov

echoTest "TEST-ARG: 01/OK - zobrazenie help spravy"
$INTERPRET $FLAGS $FILE --help
check "OK"
echo $SEPARATOR

echoTest "TEST-ARG: 02/ERR - help s inym prepinacom "
$INTERPRET $FLAGS $FILE --help -i
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 03/ERR - help s prepinacom help"
$INTERPRET $FLAGS $FILE --help --help
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 04/ERR - neznamy prepinac"
$INTERPRET $FLAGS $FILE --unknown
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 05/ERR - kontrola vstupneho suboru - neexistujuci subor"
$INTERPRET $FLAGS $FILE --input=neexistujuci
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 06/OK - zobrazenie help spravy - existujuci subor"
$INTERPRET $FLAGS $FILE --input=${INPUT_FILE}
check "OK"
echo $SEPARATOR

if [ $MERLIN_TEST -eq 0 ] ; then
echoTest "TEST-ARG: 07/ERR - prepinac input s platnym suborom ale nedostatocnym opravnenim na citanie"
sudo touch $TMP_FILE
sudo chmod o-r $TMP_FILE
$INTERPRET $FLAGS $FILE --input=$TMP_FILE
check "ERR"
echo $SEPARATOR
sudo rm $TMP_FILE
fi

echoTest "TEST-ARG: 08/OK - podpora prepinaca: -n"
$INTERPRET $FLAGS $FILE -n --input=$INPUT_FILE
check "OK"
echo $SEPARATOR

echoTest "TEST-ARG: 09/ERR - podpora prepinaca: -n=5"
$INTERPRET $FLAGS $FILE -n=5
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 10/OK - podpora prepinaca: -r=tag"
$INTERPRET $FLAGS $FILE -r=tag --input=$INPUT_FILE
check "OK"
echo $SEPARATOR

echoTest "TEST-ARG: 11/ERR - podpora prepinaca: -r"
$INTERPRET $FLAGS $FILE -r
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 12/ERR30 - podpora prepinaca: -r=\"tag tag\""
$INTERPRET $FLAGS $FILE -r="tag tag"
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 13/OK - podpora prepinaca: -s=\",\""
$INTERPRET $FLAGS $FILE -s="," --input=$INPUT_FILE
check "OK"
echo $SEPARATOR

echoTest "TEST-ARG: 13/OK - podpora prepinaca: -s=\"TAB\""
$INTERPRET $FLAGS $FILE -s="TAB" --input=$INPUT_FILE
check "OK"
echo $SEPARATOR

echoTest "TEST-ARG: 13/ERR - podpora prepinaca: -s=\"TABTAB\""
$INTERPRET $FLAGS $FILE -s="TABTAB" --input=$INPUT_FILE
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 14/OK - podpora prepinaca: -h=-"
$INTERPRET $FLAGS $FILE -h="-" --input=$INPUT_FILE
check "OK"
echo $SEPARATOR

echoTest "TEST-ARG: 15/OK - podpora prepinaca: -h"
$INTERPRET $FLAGS $FILE -h --input=$INPUT_FILE
check "OK"
echo $SEPARATOR

echoTest "TEST-ARG: 16/ERR31 - podpora prepinaca: -h=<"
$INTERPRET $FLAGS $FILE -h="<" --input=$INPUT_FILE
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 17/OK - podpora prepinaca: -c=coll"
$INTERPRET $FLAGS $FILE -c=coll  --input=$INPUT_FILE
check "OK"
echo $SEPARATOR

echoTest "TEST-ARG: 18/ERR30 - podpora prepinaca: -c=\"<\""
$INTERPRET $FLAGS $FILE -c="<"
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 19/OK - podpora prepinaca: -l=rows"
$INTERPRET $FLAGS $FILE -l=rows --input=$INPUT_FILE
check "OK"
echo $SEPARATOR

echoTest "TEST-ARG: 20/ERR30 - podpora prepinaca: -l=\"<\""
$INTERPRET $FLAGS $FILE -l="<"
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 21/OK - podpora prepinaca: -i -l=rows"
$INTERPRET $FLAGS $FILE -i -l=rows --input=$INPUT_FILE
check "OK"
echo $SEPARATOR

echoTest "TEST-ARG: 22/ERR - podpora prepinaca: -i=4"
$INTERPRET $FLAGS $FILE -i=4
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 23/ERR - podpora prepinaca: -i"
$INTERPRET $FLAGS $FILE -i
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 24/OK - podpora prepinaca: --start=10 -i -l=rows"
$INTERPRET $FLAGS $FILE --start=10 -i -l=rows --input=$INPUT_FILE
check "OK"
echo $SEPARATOR

echoTest "TEST-ARG: 25/ERR - podpora prepinaca: --start=10"
$INTERPRET $FLAGS $FILE --start=10
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 26/ERR - podpora prepinaca: --start"
$INTERPRET $FLAGS $FILE --start
check "ERR"
echo $SEPARATOR


echoTest "TEST-ARG: 27/OK - podpora prepinaca: -e"
$INTERPRET $FLAGS $FILE -e --input=$INPUT_FILE
check "OK"
echo $SEPARATOR
echoTest "TEST-ARG: 28/OK - podpora prepinaca: --error-recovery"
$INTERPRET $FLAGS $FILE --error-recovery --input=$INPUT_FILE
check "OK"
echo $SEPARATOR

echoTest "TEST-ARG: 29/ERR - podpora prepinaca: -e=10"
$INTERPRET $FLAGS $FILE -e=10
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 30/OK - podpora prepinaca: --missing-field=vals -e"
$INTERPRET $FLAGS $FILE --missing-field=vals -e --input=$INPUT_FILE
check "OK"
echo $SEPARATOR

echoTest "TEST-ARG: 31/OK - podpora prepinaca: --missing-field=\"<tag>\" -e"
$INTERPRET $FLAGS $FILE --missing-field="<tag>" -e --input=$INPUT_FILE
check "OK"
echo $SEPARATOR

echoTest "TEST-ARG: 32/ERR - podpora prepinaca: --missing-field=vals"
$INTERPRET $FLAGS $FILE --missing-field=vals
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 33/ERR - podpora prepinaca: --missing-field=\"<tag>\""
$INTERPRET $FLAGS $FILE --missing-field="<tag>"
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 34/OK - podpora prepinaca: --all-columns -e"
$INTERPRET $FLAGS $FILE --all-columns -e --input=$INPUT_FILE
check "OK"
echo $SEPARATOR

echoTest "TEST-ARG: 35/ERR - podpora prepinaca: --all-columns"
$INTERPRET $FLAGS $FILE --all-columns
check "ERR"
echo $SEPARATOR

echoTest "TEST-ARG: 36/ERR - podpora prepinaca: --all-columns=rows"
$INTERPRET $FLAGS $FILE --all-columns=rows
check "ERR"
echo $SEPARATOR

# Koniec testov argumentov
##################################

##################################
# Zaciatok testu XML parsoanie
echoTest "TEST-XML: 01/OK - ${REF_OUTPUT_DIR}/test01.csv -r=root"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test01.csv -r=root --output=$JEXAMXML_INPUT
check "OK"
$JEXAMXML_INTERPRET $JEXAMXML_INPUT ${REF_OUTPUT_DIR}/test01a.xml $JEXAMXML_DELTA  $JEXAMXML_OPTIONS
check "OK"
echo $SEPARATOR

echoTest "TEST-XML: 02/OK - ${REF_OUTPUT_DIR}/test02.csv -r=root -l=\"ra-dek\""
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test02.csv -r=root -l="ra-dek" --output=$JEXAMXML_INPUT
check "OK"
$JEXAMXML_INTERPRET $JEXAMXML_INPUT ${REF_OUTPUT_DIR}/test02a.xml $JEXAMXML_DELTA  $JEXAMXML_OPTIONS
check "OK"
echo $SEPARATOR

echoTest "TEST-XML: 03/OK - ${REF_OUTPUT_DIR}/test03.csv -l=radek -r=root -i -n"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test03.csv --output=$JEXAMXML_INPUT -l=radek -r=root -i -n
check "OK"
$JEXAMXML_INTERPRET $JEXAMXML_INPUT ${REF_OUTPUT_DIR}/test03.xml $JEXAMXML_DELTA  $JEXAMXML_OPTIONS
check "OK"
echo $SEPARATOR

echoTest "TEST-XML: 04/ERR - ${REF_OUTPUT_DIR}/test04.in -s=\;"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test04.csv -s=\;
check "ERR"
echo $SEPARATOR

echoTest "TEST-XML: 05/OK - ${REF_OUTPUT_DIR}/test05.csv -r=root -s=\; -e"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test05.csv --output=$JEXAMXML_INPUT -r=root -s=\; -e
check "OK"
$JEXAMXML_INTERPRET $JEXAMXML_INPUT ${REF_OUTPUT_DIR}/test05a.xml $JEXAMXML_DELTA  $JEXAMXML_OPTIONS
check "OK"
echo $SEPARATOR

echoTest "TEST-XML: 06/OK - ${REF_OUTPUT_DIR}/test06.csv --start=2 -h -i -l=data"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test06.csv --output=$JEXAMXML_INPUT --start=2 -h -i -l=data
check "OK"
$JEXAMXML_INTERPRET $JEXAMXML_INPUT ${REF_OUTPUT_DIR}/test06.xml $JEXAMXML_DELTA  $JEXAMXML_OPTIONS
check "OK"
echo $SEPARATOR

echoTest "TEST-XML: 07/OK - ${REF_OUTPUT_DIR}/test07.csv "
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test07.csv --output=$JEXAMXML_INPUT
check "OK"
$JEXAMXML_INTERPRET $JEXAMXML_INPUT ${REF_OUTPUT_DIR}/test07.xml $JEXAMXML_DELTA  $JEXAMXML_OPTIONS
check "OK"
echo $SEPARATOR

echoTest "TEST-XML: 08/OK - ${REF_OUTPUT_DIR}/test08.csv -r=root"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test08.csv --output=$JEXAMXML_INPUT -r=root
check "OK"
$JEXAMXML_INTERPRET $JEXAMXML_INPUT ${REF_OUTPUT_DIR}/test08a.xml $JEXAMXML_DELTA  $JEXAMXML_OPTIONS
check "OK"
echo $SEPARATOR

echoTest "TEST-XML: 09/OK - ${REF_OUTPUT_DIR}/test09.csv -r=róót -h"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test09.csv -r=róót -h --output=$JEXAMXML_INPUT
check "OK"
$JEXAMXML_INTERPRET $JEXAMXML_INPUT ${REF_OUTPUT_DIR}/test09.xml $JEXAMXML_DELTA  $JEXAMXML_OPTIONS
check "OK"
echo $SEPARATOR

echoTest "TEST-XML: 10/OK - ${REF_OUTPUT_DIR}/test10.csv -r=root -h"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test10.csv --output=$JEXAMXML_INPUT -r=root -h
check "OK"
$JEXAMXML_INTERPRET $JEXAMXML_INPUT ${REF_OUTPUT_DIR}/test10a.xml $JEXAMXML_DELTA  $JEXAMXML_OPTIONS
check "OK"
echo $SEPARATOR

echoTest "TEST-XML: 11/OK - ${REF_OUTPUT_DIR}/test11.csv -l=line -i"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test11.csv --output=$JEXAMXML_INPUT -l=line -i
check "OK"
$JEXAMXML_INTERPRET $JEXAMXML_INPUT ${REF_OUTPUT_DIR}/test11.xml $JEXAMXML_DELTA  $JEXAMXML_OPTIONS
check "OK"
echo $SEPARATOR

echoTest "TEST-XML: 12/ERR - ${REF_OUTPUT_DIR}/test12.in -h"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test12.csv -h;
check "ERR"
echo $SEPARATOR

echoTest "TEST-XML: 13/OK - ${REF_OUTPUT_DIR}/test13.csv -r=root --start=5 -i -s=TAB -l=row"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test13.csv --output=$JEXAMXML_INPUT -r=root --start=5 -i -s=TAB -l=row
check "OK"
$JEXAMXML_INTERPRET $JEXAMXML_INPUT ${REF_OUTPUT_DIR}/test13a.xml $JEXAMXML_DELTA  $JEXAMXML_OPTIONS
check "OK"
echo $SEPARATOR

echoTest "TEST-XML: 14/OK - ${REF_OUTPUT_DIR}/test14.csv -s=TAB -h"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test14.csv --output=$JEXAMXML_INPUT -s=TAB -h
check "OK"
$JEXAMXML_INTERPRET $JEXAMXML_INPUT ${REF_OUTPUT_DIR}/test14.xml $JEXAMXML_DELTA  $JEXAMXML_OPTIONS
check "OK"
echo $SEPARATOR

echoTest "TEST-XML: 15/ERR32 - ${REF_OUTPUT_DIR}/test15.in -r=root"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test15.csv -r=root;
check "ERR"
echo $SEPARATOR

echoTest "TEST-XML: 16/OK - ${REF_OUTPUT_DIR}/test16.csv --all-columns -e -r=root"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test16.csv --output=$JEXAMXML_INPUT --all-columns -e -r=root
check "OK"
$JEXAMXML_INTERPRET $JEXAMXML_INPUT ${REF_OUTPUT_DIR}/test16.xml $JEXAMXML_DELTA  $JEXAMXML_OPTIONS
check "OK"
echo $SEPARATOR

echoTest "TEST-XML: 17/ERR30 - ${REF_OUTPUT_DIR}/test17.in  --error-recovery -r=\" root\""
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test17.csv --error-recovery -r=" root";
check "ERR"
echo $SEPARATOR

echoTest "TEST-XML: 18/OK - ${REF_OUTPUT_DIR}/test18.csv -e -h=jk --all-columns --missing-field=chybí -c=Kontakt -r=root"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test18.csv --output=$JEXAMXML_INPUT -e -h=jk --all-columns --missing-field=chybí -c=Kontakt -r=root
check "OK"
$JEXAMXML_INTERPRET $JEXAMXML_INPUT ${REF_OUTPUT_DIR}/test18a.xml $JEXAMXML_DELTA  $JEXAMXML_OPTIONS
check "OK"
echo $SEPARATOR




# Koniec testov XML parsovania
##################################


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
