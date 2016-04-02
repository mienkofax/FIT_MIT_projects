CC=g++
FLAGS= -Wall -pedantic -Wextra

LOGIN=xtisov00

all: webclient.cpp
	$(CC) $(FLAGS) webclient.cpp -o webclient

tar: tar
	tar -cf $(LOGIN).tar webclient.cpp Makefile
	gzip $(LOGIN).tar
