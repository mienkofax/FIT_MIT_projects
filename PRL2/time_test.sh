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
AVG=0
for k in $(seq 0 9)
do
	OUT="$(${BINNARY} ${INPUTS[$i]})"
	TMP=$( echo $OUT | grep -o -E '[0-9]+.[0-9]*')
	AVG=`echo $AVG + $TMP | bc`
done
echo `python -c "print $AVG / 10"`
done
