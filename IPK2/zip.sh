#!/bin/bash

rm -f xtisov00.tar.gz


#vytvorenie archivu najpr tar potom gz
tar -cf xtisov00.tar client.cpp common.cpp common.h server.cpp Makefile README dokumentace.pdf test.sh
gzip xtisov00.tar

#upratanie 
cd Dokumentace 
make cleanall
cd ..
