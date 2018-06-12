#!/bin/bash

VALUES=$1
BIN_NAME=pro

if [ $# -lt 1 ]; then
	echo "Invalid input."
	exit 1
fi

INPUT_SIZE=${#VALUES}
CPU=$((INPUT_SIZE * 2 - 2))

if [ "${1}" == "c" ]; then
	mpic++ --prefix /usr/local/share/OpenMPI -std=c++11 -o ${BIN_NAME} pro.cpp
	exit 1
fi

mpirun --prefix /usr/local/share/OpenMPI -np ${CPU} ${BIN_NAME} ${VALUES}
