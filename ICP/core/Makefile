CXX=g++
CFLAGS=-Wall -pedantic -std=c++11

all: library

test:
	$(CXX) -Wall -pedantic  main.cpp cli.cpp GameBoard.cpp GameData.cpp GameManager.cpp Player.cpp Strategy.cpp -o test --std=c++11

library:
	g++ -c GameManager.cpp GameBoard.cpp GameData.cpp Strategy.cpp Player.cpp -Wall -pedantic --std=c++11
	ar -rv othello-core.a GameManager.o
	ar -rv othello-core.a GameData.o
	ar -rv othello-core.a GameBoard.o
	ar -rv othello-core.a Player.o
	ar -rv othello-core.a Strategy.o

cli:
	g++ main.cpp cli.cpp othello-core.a -o cli --std=c++11 -lm -pedantic -Wall
