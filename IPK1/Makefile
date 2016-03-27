CC=g++
FLAGS= -Wall -pedantic -Wextra

LOGIN=xtisov00

all: webclient.cpp
	$(CC) $(FLAGS) webclient.cpp -o webclient

zip:
	zip $(LOGIN).zip Makefile webclient.cpp
