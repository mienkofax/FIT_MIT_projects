#!/bin/bash
# Peter Tisovcik (xtisov00)

VALUES=$1
BIN_NAME=pro

if [ $# -ne 1 ]; then
	echo "Invalid input."
	exit 1
fi

INPUT_SIZE=${#VALUES}
CPU=$((INPUT_SIZE * 2 - 2))
if [ "${CPU}" -le "0" ]; then
	CPU=1
fi

mpic++ --prefix /usr/local/share/OpenMPI -std=c++11 -o ${BIN_NAME} pro.cpp
mpirun --prefix /usr/local/share/OpenMPI -np ${CPU} ${BIN_NAME} "${VALUES}"

rm -f ${BIN_NAME}
