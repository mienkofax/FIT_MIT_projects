#!/bin/bash

ERROR=0
SEPARATOR="-------------------------------------------"

OUTPUT_FILE0=file.html
OUTPUT_FILE1=file.html

#stiahnutie referencneho suboru

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

make

echoTest "TEST-01/ERR - program bez parametrov"
./webclient
check "ERR"
echo $SEPARATOR

echoTest "TEST-02/ERR - test odkazu bez http"
./webclient www.google.sk
check "ERR"
echo $SEPARATOR

echoTest "TEST-03/ERR - neexistujuci server"
./webclient http://www.googlesssss.sk
check "ERR"
echo $SEPARATOR

echoTest "TEST-04/OK - test url so spravnym odkazom"
./webclient http://www.google.sk
check "OK"
echo $SEPARATOR

echoTest "TEST-05/OK - test url - http://www.fit.vutbr.cz"
./webclient http://www.fit.vutbr.cz
check "OK"
echo $SEPARATOR

echoTest "TEST-06/OK - test url - http://www.fit.vutbr.cz/"
./webclient http://www.fit.vutbr.cz/
check "OK"
echo $SEPARATOR

echoTest "TEST-07/OK - test url - http://www.fit.vutbr.cz/news"
./webclient http://www.fit.vutbr.cz/news
check "OK"
echo $SEPARATOR

echoTest "TEST-08/OK - test url - http://www.fit.vutbr.cz:80"
./webclient http://www.fit.vutbr.cz:80
check "OK"
echo $SEPARATOR

echoTest "TEST-09/OK - test url - http://www.fit.vutbr.cz:80/"
./webclient http://www.fit.vutbr.cz:80/
check "OK"
echo $SEPARATOR

echoTest "TEST-10/OK - test url - http://www.fit.vutbr.cz:80/news"
./webclient http://www.fit.vutbr.cz:80/news
check "OK"
echo $SEPARATOR

echoTest "TEST-11/OK - test url - http://www.fit.vutbr.cz:80/news/"
./webclient http://www.fit.vutbr.cz:80/news/
check "OK"
echo $SEPARATOR

echoTest "TEST-12/OK - stáhne defaultní stránku a uloží do souboru index.html v aktuálním"
./webclient http://www.fit.vutbr.cz
check "OK"
echo $SEPARATOR

echoTest "TEST-13/OK - stáhne a uloží obrázek fit_logo_cz.gif do souboru v aktuálním adresáři"
./webclient http://www.fit.vutbr.cz:80/common/img/fit_logo_cz.gif
check "OK"
echo $SEPARATOR

echoTest "TEST-14/OK - stáhne a uloží soubor some text.txt do souboru v aktuálním adresáři"
./webclient "http://www.fit.vutbr.cz/study/courses/IPK/public/some text.txt"
check "OK"
echo $SEPARATOR
exit
echoTest "TEST-15/OK - stáhne a uloží soubor, na který byla přesměrována odpověď"
./webclient http://www.fit.vutbr.cz/study/courses/IPK/public/test/redir.php
check "OK"
echo $SEPARATOR


if [ $ERROR -eq 0 ] ; then
	echo -ne "\e[92m"
else
	echo -ne "\e[31m"
fi

#pocet chyb
echo "POCET CHYB: $ERROR"
echo -n -e "\e[39m" #reset farby

echo $SEPARATOR
