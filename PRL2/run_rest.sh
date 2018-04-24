#!/bin/bash

./fast_test.sh 'c'

BINNARY=./fast_test.sh
INPUTS=(
	A
	AB
	ABC
	ABCD
	ABCDE
	ABCDEF
	ABCDEFG
	ABCDEFGH
	ABCDEFGHI
	ABCDEFGHIJ
	ABCDEFGHIJK
	ABCDEFGHIJKL
	ABCDEFGHIJKLM
	ABCDEFGHIJKLMO
	ABCDEFGHIJKLMOP
	ABCDEFGHIJKLMOPR
	ABCDEFGHIJKLMOPRS
)

OUTPUTS=(
	A
	AB
	ABC
	ABDC
	ABDEC
	ABDECF
	ABDECFG
	ABDHECFG
	ABDHIECFG
	ABDHIEJCFG
	ABDHIEJKCFG
	ABDHIEJKCFLG
	ABDHIEJKCFLMG
	ABDHIEJKCFLMGO
	ABDHIEJKCFLMGOP
	ABDHRIEJKCFLMGOP
	ABDHRSIEJKCFLMGOP
)

if [ "${#INPUTS[@]}" -ne "${#OUTPUTS[@]}" ]; then
	echo "Length of input and output array is different."
	exit 1
fi

INPUT_LENGTH="${#INPUTS[@]}"
if [ "merlin.fit.vutbr.cz" == `hostname` ]; then
	INPUT_LENGTH=11
	echo "Tests are limited to first ${INPUT_LENGTH} items on Merlin."
fi

for i in $(seq 0 $((INPUT_LENGTH - 1)))
do
	OUT="$(${BINNARY} ${INPUTS[$i]})"
	if [ "${OUT}" == "${OUTPUTS[$i]}" ]; then
		echo -ne "\e[92m"
		echo -n "OK  "
	else
		echo -ne "\e[31m"
		echo -n "ERR "
	fi

	echo -n -e "\e[36m"
	echo -en "\t I: ${OUTPUTS[$i]}"
	echo -en "\t O: ${OUT}"
	echo -n -e "\e[39m"
	echo ""
done
