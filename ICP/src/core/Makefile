CXX=g++
CXXFLAGS=-Wall -pedantic -std=c++11
LIBRARY_NAME=othello-lib.a

all: library

library:
	g++ -c $(CXXFLAGS) GameManager.cpp GameBoard.cpp GameData.cpp Strategy.cpp Player.cpp
	ar rcs $(LIBRARY_NAME) GameManager.o GameData.o GameBoard.o Player.o Strategy.o

clean:
	rm -f *.o

cleanall: clean
	rm -f $(LIBRARY_NAME)
