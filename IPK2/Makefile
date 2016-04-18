CC=g++
FLAGS= -Wall -pedantic -Wextra

LOGIN=xtisov00
FILES= Makefile server.cpp client.cpp common.cpp common.h test.sh READNE

all: clean server client

server: clean
	$(CC) server.cpp common.cpp -o server

client: clean
	$(CC) client.cpp common.cpp -o client
clean:
	rm -f server xtisov00.tar.gz

tar: tar
		tar -cf $(LOGIN).tar $(FILES)
		gzip $(LOGIN).tar
