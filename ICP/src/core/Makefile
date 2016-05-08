#
# @file			Makefile
# @author		Klára Nečasová <xnecas24>
# @author		Peter Tisovčík <xtisov00>
#

CXX=g++
CXXFLAGS=-Wall -pedantic -std=c++11
LIBRARY_NAME=othello-lib.a

all: library

#vytvorenie kniznice
library:
	g++ -c $(CXXFLAGS) GameManager.cpp GameBoard.cpp GameData.cpp Strategy.cpp Player.cpp
	ar rcs $(LIBRARY_NAME) GameManager.o GameData.o GameBoard.o Player.o Strategy.o

#odstranenie nepotrebnych suborov
clean:
	rm -f *.o

#odstranenie vsetkych veci co sa vytvorili
cleanall: clean
	rm -f $(LIBRARY_NAME)
