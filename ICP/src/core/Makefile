CXX=g++
CXXFLAGS=-Wall -pedantic -std=c++11
LIBRARY_NAME=othello-lib.a



library:
	g++ -c $(CXXFLAGS) GameManager.cpp GameBoard.cpp GameData.cpp Strategy.cpp Player.cpp
	ar -rv $(LIBRARY_NAME) GameManager.o
	ar -rv $(LIBRARY_NAME) GameData.o
	ar -rv $(LIBRARY_NAME) GameBoard.o
	ar -rv $(LIBRARY_NAME) Player.o
	ar -rv $(LIBRARY_NAME) Strategy.o

clean:
	rm -f GameManager.o GameBoard.o GameData.o Strategy.o Player.o

cleanall: clean
	rm -f $(LIBRARY_NAME)
