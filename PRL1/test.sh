#!/bin/bash
# Peter Tisovčík (xtisov00)

NUMBERS=$1
PROCESSORS=$2

if [ $# -ne 2 ]; then
	echo "neboli zadane argumenty"
	exit 1
fi

if [ ${NUMBERS} -lt 1 ]; then
	echo "nevalidny pocet cisiel"
	exit 2
fi

if [ ${PROCESSORS} -lt 1 ]; then
	echo "nevalidny pocet procesorov"
	exit 2
fi

mpic++ --prefix /usr/local/share/OpenMPI -std=c++11 -o mss mss.cpp
dd if=/dev/random bs=1 count=${NUMBERS} of=numbers 2>/dev/null
mpirun --prefix /usr/local/share/OpenMPI -np ${PROCESSORS} mss

rm -f mss numbers

