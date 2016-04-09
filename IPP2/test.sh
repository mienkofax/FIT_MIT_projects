#!/bin/bash

INTERPRET=python3
FILE=csvPeto.py

JEXAMXML_INTERPRET="java -jar jexamxml.jar"
JEXAMXML_OPTIONS=csv_options
JEXAMXML_INPUT=output.xml
JEXAMXML_DELTA=delta.xml

TEST_DIR="testy"
REF_INPUT_DIR=$TEST_DIR
REF_OUTPUT_DIR=$TEST_DIR"/ref-out"
ERROR=0
ERR_CODE=0
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

checkErrCode() {
	if [ $1 != $ERR_CODE ] ; then
		echo -ne "\e[31m"
		echo "Zly navratovy kod"
		echo -n -e "\e[39m"
		ERROR=$((ERROR + 1)) #pocet chyb
		echo $ERR_CODE
	fi
}

check () {
	ERR=$?
	ERR_CODE=$ERR
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

csvCheck() {
	if [ $2 == "0" ] ; then
		echoTest "TEST-XML: $1/OK - ${REF_OUTPUT_DIR}/$1.csv $3"
		#echoTest " - "
		$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/$1.csv --output=$JEXAMXML_INPUT $3
		check "OK"
		$JEXAMXML_INTERPRET $JEXAMXML_INPUT ${REF_OUTPUT_DIR}/$1.xml $JEXAMXML_DELTA  $JEXAMXML_OPTIONS
		check "OK"
		echo $SEPARATOR
	else
		echoTest "TEST-XML: $1/ERR - ${REF_OUTPUT_DIR}/$1.csv  $3"
		#echoTest " - "
		$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/$1.csv --output=$JEXAMXML_INPUT $3;
		check "ERR"
		checkErrCode $2
		echo $SEPARATOR
	fi
}

echo $SEPARATOR

##################################
# Test argumentov

echoTest "TEST-ARG: 01/OK - zobrazenie help spravy"
$INTERPRET $FLAGS $FILE --help
check "OK"
checkErrCode 0
echo $SEPARATOR

echoTest "TEST-ARG: 02/ERR - help s inym prepinacom "
$INTERPRET $FLAGS $FILE --help -i
check "ERR"
checkErrCode 1
echo $SEPARATOR

echoTest "TEST-ARG: 03/ERR - help s prepinacom help"
$INTERPRET $FLAGS $FILE --help --help
check "ERR"
checkErrCode 1
echo $SEPARATOR

echoTest "TEST-ARG: 04/ERR - neznamy prepinac"
$INTERPRET $FLAGS $FILE --unknown
check "ERR"
checkErrCode 1
echo $SEPARATOR

echoTest "TEST-ARG: 05/ERR - kontrola vstupneho suboru - neexistujuci subor"
$INTERPRET $FLAGS $FILE --input=neexistujuci
check "ERR"
checkErrCode 2
echo $SEPARATOR

echoTest "TEST-ARG: 06/OK - zobrazenie help spravy - existujuci subor"
$INTERPRET $FLAGS $FILE --input=${INPUT_FILE}
check "OK"
checkErrCode 0
echo $SEPARATOR

if [ $MERLIN_TEST -eq 0 ] ; then
echoTest "TEST-ARG: 07/ERR - prepinac input s platnym suborom ale nedostatocnym opravnenim na citanie"
sudo touch $TMP_FILE
sudo chmod o-r $TMP_FILE
$INTERPRET $FLAGS $FILE --input=$TMP_FILE
check "ERR"
checkErrCode 2
echo $SEPARATOR
sudo rm $TMP_FILE
fi

echoTest "TEST-ARG: 08/OK - podpora prepinaca: -n"
$INTERPRET $FLAGS $FILE -n --input=$INPUT_FILE
check "OK"
checkErrCode 0
echo $SEPARATOR

echoTest "TEST-ARG: 09/ERR - podpora prepinaca: -n=5"
$INTERPRET $FLAGS $FILE -n=5
check "ERR"
checkErrCode 1
echo $SEPARATOR

echoTest "TEST-ARG: 10/OK - podpora prepinaca: -r=tag"
$INTERPRET $FLAGS $FILE -r=tag --input=$INPUT_FILE
check "OK"
checkErrCode 0
echo $SEPARATOR

echoTest "TEST-ARG: 11/ERR - podpora prepinaca: -r"
$INTERPRET $FLAGS $FILE -r
check "ERR"
checkErrCode 1
echo $SEPARATOR

echoTest "TEST-ARG: 12/ERR30 - podpora prepinaca: -r=\"tag tag\""
$INTERPRET $FLAGS $FILE -r="tag tag"
check "ERR"
checkErrCode 30
echo $SEPARATOR

echoTest "TEST-ARG: 13/OK - podpora prepinaca: -s=\",\""
$INTERPRET $FLAGS $FILE -s="," --input=$INPUT_FILE
check "OK"
checkErrCode 0
echo $SEPARATOR

echoTest "TEST-ARG: 13/OK - podpora prepinaca: -s=\"TAB\""
$INTERPRET $FLAGS $FILE -s="TAB" --input=$INPUT_FILE
check "OK"
checkErrCode 0
echo $SEPARATOR

echoTest "TEST-ARG: 13/ERR - podpora prepinaca: -s=\"TABTAB\""
$INTERPRET $FLAGS $FILE -s="TABTAB" --input=$INPUT_FILE
check "ERR"
checkErrCode 1
echo $SEPARATOR

echoTest "TEST-ARG: 14/OK - podpora prepinaca: -h=-"
$INTERPRET $FLAGS $FILE -h="-" --input=$INPUT_FILE
check "OK"
checkErrCode 0
echo $SEPARATOR

echoTest "TEST-ARG: 15/OK - podpora prepinaca: -h"
$INTERPRET $FLAGS $FILE -h --input=$INPUT_FILE
check "OK"
checkErrCode 0
echo $SEPARATOR

echoTest "TEST-ARG: 16/OK - podpora prepinaca: -h=<"
$INTERPRET $FLAGS $FILE -h="<" --input=$INPUT_FILE
check "OK"
checkErrCode 0
echo $SEPARATOR

echoTest "TEST-ARG: 17/OK - podpora prepinaca: -c=coll"
$INTERPRET $FLAGS $FILE -c=coll  --input=$INPUT_FILE
check "OK"
checkErrCode 0
echo $SEPARATOR

echoTest "TEST-ARG: 18/ERR30 - podpora prepinaca: -c=\"<\""
$INTERPRET $FLAGS $FILE -c="<"
check "ERR"
checkErrCode 30
echo $SEPARATOR

echoTest "TEST-ARG: 19/OK - podpora prepinaca: -l=rows"
$INTERPRET $FLAGS $FILE -l=rows --input=$INPUT_FILE
check "OK"
checkErrCode 0
echo $SEPARATOR

echoTest "TEST-ARG: 20/ERR30 - podpora prepinaca: -l=\"<\""
$INTERPRET $FLAGS $FILE -l="<"
check "ERR"
checkErrCode 30
echo $SEPARATOR

echoTest "TEST-ARG: 21/OK - podpora prepinaca: -i -l=rows"
$INTERPRET $FLAGS $FILE -i -l=rows --input=$INPUT_FILE
check "OK"
checkErrCode 0
echo $SEPARATOR

echoTest "TEST-ARG: 22/ERR - podpora prepinaca: -i=4"
$INTERPRET $FLAGS $FILE -i=4
check "ERR"
checkErrCode 1
echo $SEPARATOR

echoTest "TEST-ARG: 23/ERR - podpora prepinaca: -i"
$INTERPRET $FLAGS $FILE -i
check "ERR"
checkErrCode 1
echo $SEPARATOR

echoTest "TEST-ARG: 24/OK - podpora prepinaca: --start=10 -i -l=rows"
$INTERPRET $FLAGS $FILE --start=10 -i -l=rows --input=$INPUT_FILE
check "OK"
checkErrCode 0
echo $SEPARATOR

echoTest "TEST-ARG: 25/ERR - podpora prepinaca: --start=10"
$INTERPRET $FLAGS $FILE --start=10
check "ERR"
checkErrCode 1
echo $SEPARATOR

echoTest "TEST-ARG: 26/ERR - podpora prepinaca: --start"
$INTERPRET $FLAGS $FILE --start
check "ERR"
checkErrCode 1
echo $SEPARATOR


echoTest "TEST-ARG: 27/OK - podpora prepinaca: -e"
$INTERPRET $FLAGS $FILE -e --input=$INPUT_FILE
check "OK"
checkErrCode 0
echo $SEPARATOR

echoTest "TEST-ARG: 28/OK - podpora prepinaca: --error-recovery"
$INTERPRET $FLAGS $FILE --error-recovery --input=$INPUT_FILE
check "OK"
checkErrCode 0
echo $SEPARATOR

echoTest "TEST-ARG: 29/ERR - podpora prepinaca: -e=10"
$INTERPRET $FLAGS $FILE -e=10
check "ERR"
checkErrCode 1
echo $SEPARATOR

echoTest "TEST-ARG: 30/OK - podpora prepinaca: --missing-field=vals -e"
$INTERPRET $FLAGS $FILE --missing-field=vals -e --input=$INPUT_FILE
check "OK"
checkErrCode 0
echo $SEPARATOR

echoTest "TEST-ARG: 31/OK - podpora prepinaca: --missing-field=\"<tag>\" -e"
$INTERPRET $FLAGS $FILE --missing-field="<tag>" -e --input=$INPUT_FILE
check "OK"
checkErrCode 0
echo $SEPARATOR

echoTest "TEST-ARG: 32/ERR - podpora prepinaca: --missing-field=vals"
$INTERPRET $FLAGS $FILE --missing-field=vals
check "ERR"
checkErrCode 1
echo $SEPARATOR

echoTest "TEST-ARG: 33/ERR - podpora prepinaca: --missing-field=\"<tag>\""
$INTERPRET $FLAGS $FILE --missing-field="<tag>"
check "ERR"
checkErrCode 1
echo $SEPARATOR

echoTest "TEST-ARG: 34/OK - podpora prepinaca: --all-columns -e"
$INTERPRET $FLAGS $FILE --all-columns -e --input=$INPUT_FILE
check "OK"
checkErrCode 0
echo $SEPARATOR

echoTest "TEST-ARG: 35/ERR - podpora prepinaca: --all-columns"
$INTERPRET $FLAGS $FILE --all-columns
check "ERR"
checkErrCode 1
echo $SEPARATOR

echoTest "TEST-ARG: 36/ERR - podpora prepinaca: --all-columns=rows"
$INTERPRET $FLAGS $FILE --all-columns=rows
check "ERR"
checkErrCode 1
echo $SEPARATOR

# Koniec testov argumentov
##################################

##################################
# Zaciatok testov XML parsoanie

#skolske testy
csvCheck "test01" 0 "-r=root"
csvCheck "test02" 0 "-r=root -l=ra-dek"
csvCheck "test03" 0 "-l=radek -r=root -i -n"

echoTest "TEST-XML: 04/ERR - ${REF_OUTPUT_DIR}/test04.in -s=\;"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test04.csv -s=\;
check "ERR"
checkErrCode 32
echo $SEPARATOR

echoTest "TEST-XML: 05/OK - ${REF_OUTPUT_DIR}/test05.csv -r=root -s=\; -e"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test05.csv --output=$JEXAMXML_INPUT -r=root -s=\; -e
check "OK"
$JEXAMXML_INTERPRET $JEXAMXML_INPUT ${REF_OUTPUT_DIR}/test05.xml $JEXAMXML_DELTA  $JEXAMXML_OPTIONS
check "OK"
checkErrCode 0
echo $SEPARATOR

csvCheck "test06" 0 "--start=2 -h -i -l=data"
csvCheck "test07" 0 ""
csvCheck "test08" 0 "-r=root"
csvCheck "test09" 0 "-r=róót -h"
csvCheck "test10" 0 "-r=root -h"

csvCheck "test11" 0 "-l=line -i"
csvCheck "test12" 31 "-h"
csvCheck "test13" 0 "-r=root --start=5 -i -s=TAB -l=row"
csvCheck "test14" 0 "-s=TAB -h"
csvCheck "test15" 32 "-r=root"
csvCheck "test16" 0 "-r=root --all-columns -e"

echo "haha"
echoTest "TEST-XML: 17/ERR30 - ${REF_OUTPUT_DIR}/test17.in  --error-recovery -r=\" root\""
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test17.csv --error-recovery -r=" root";
check "ERR"
checkErrCode 30
echo $SEPARATOR

csvCheck "test18" 0 "-r=root -e -h=jk --all-columns --missing-field=chybí -c=Kontakt"

#testy 2
csvCheck "test21" 0 "-r=root"
csvCheck "test22" 0 "-r=root -n"
csvCheck "test23" 0 "-r=root -h"
csvCheck "test24" 0 "-r=root -s=,"
csvCheck "test25" 0 "-r=root -s=;"
csvCheck "test26" 0 "-r=root -l=radek"
csvCheck "test27" 0 "-r=root -l=řádek"
csvCheck "test28" 30 "-r=root -l=:_.-RaDek123"
csvCheck "test29" 0 "-r=root -l=row -i"
csvCheck "test30" 0 "-r=root -l=row -i --start=8"
csvCheck "test31" 0 "-r=root -l=row -i --start=0"
csvCheck "test32" 0 "-r=root -e"
csvCheck "test33" 0 "-r=root --error-recovery"
csvCheck "test34" 0 "-r=root -e -h"
csvCheck "test35" 0 "-r=root -e -h"
csvCheck "test36" 0 "-r=root -e --missing-field=chybějící_údaj"
csvCheck "test37" 0 "-r=root -e --missing-field=a<b&&c>d"
csvCheck "test38" 0 "-r=root -e -h --missing-field=žluťoučký_koník"
csvCheck "test39" 0 "-r=root -e --all-columns"
csvCheck "test40" 0 "-r=root -e -h --all-columns"
csvCheck "test41" 0 "-r=root -e --all-columns --missing-field=trol"
csvCheck "test42" 0 "-r=root -e -h --all-columns --missing-field=t"
csvCheck "test43" 0 "-r=root -e -h --all-columns --missing-field=vzoreček:ž<ý -l=čočka -i --start=15"
csvCheck "test44" 0 "-r=root -s=~ -h"
csvCheck "test45" 0 "-r=root -s=TAB -h"
csvCheck "test46" 0 "-r=root -h"
csvCheck "test47" 0 "-r=root"
csvCheck "test48" 0 "-r=root -l=ra-dek"
csvCheck "test49" 0 "-r=root -l=radek -i -n"

echoTest "TEST-XML: 50/OK - ${REF_OUTPUT_DIR}/test50.csv -r=root -s=\; -e"
echoTest " - "
$INTERPRET $FLAGS $FILE --input=$REF_INPUT_DIR/test50.csv --output=$JEXAMXML_INPUT -r=root -s=\; -e
check "OK"
$JEXAMXML_INTERPRET $JEXAMXML_INPUT ${REF_OUTPUT_DIR}/test50.xml $JEXAMXML_DELTA  $JEXAMXML_OPTIONS
check "OK"
checkErrCode 0
echo $SEPARATOR

csvCheck "test51" 0 "-r=root --start=2 -h -i -l=data"
csvCheck "test52" 0 "-r=root"
csvCheck "test53" 0 "-r=root"
csvCheck "test54" 0 "-r=róót  -h"
csvCheck "test55" 0 "-r=root -h"
csvCheck "test56" 0 "-r=root -l=line -i"
csvCheck "test57" 0 "-r=root --start=5 -i -s=TAB -l=row"
csvCheck "test58" 0 "-r=root -s=TAB -h"
csvCheck "test59" 0 "-r=root --all-columns -e"

#fituska
csvCheck "fituska1" 0 "-r=root"
csvCheck "fituska2" 0 "-l=line -i"
csvCheck "fituska3" 0 "-l=line -i"

#chybove testy
csvCheck "return1" 0 ""

csvCheck "return2" 32 ""
csvCheck "return1" 30 "-r=\&tralala"
csvCheck "return1" 30 "-c=tral&l&"
csvCheck "return1" 30 "-l=<row>"
csvCheck "return1" 1 "--help"
csvCheck "return1" 1 "-l"
csvCheck "return1" 1 "-i"
csvCheck "return1" 1 "-i --start=42"
csvCheck "return1" 1 "-l=row --start=42"
csvCheck "return1" 1 "--start=42"
csvCheck "return1" 1 "-l=hello -i --start=-42"
csvCheck "return1" 1 "-l=hello -i --start=0.5"
csvCheck "return1" 1 "-l=hello -i --start=1xtr0ll"
csvCheck "return1" 1 "--missing-field=5"
csvCheck "return1" 1 "--all-columns"



csvCheck "return1" 2 "--input=neexistujici_soubor" # pokud skript vraci chybu cislo 1, je to jeste lepsi chovani O:) (2x parametr input)
csvCheck "return3" 31 "-h=0"
csvCheck "return3" 0 "-r=root -h=x"
csvCheck "return3" 1 "-r=root -r=boot"
csvCheck "return3" 1 "--input=tests/return1.csv --input=tests/return1.csv"
csvCheck "return3" 31 "-h=&nbsp"
csvCheck "return3" 1 "--start=91 -i"
csvCheck "return3" 1 "--start=0"



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
