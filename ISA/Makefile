CC=g++
FLAGS= -std=c++11 -Wall -pedantic -Wextra
LOGIN=xtisov00
FILES=	ArgumentParser.cpp \
		ArgumentValidator.cpp \
		PcapReaderFromFile.cpp \
		PcapReaderFromVector.cpp \
		LayerMessage.cpp \
		LinkLayerMessageFactory.cpp \
		NetworkLayerMessageFactory.cpp \
		TransportLayerMessageFactory.cpp \
		GenericLayerMessageFactory.cpp \
		Statistics.cpp \
		main.cpp

OTHERS=	ArgumentParser.h \
		ArgumentValidator.h \
		PcapReaderFromFile.h \
		PcapReaderFromVector.h \
		LayerMessage.h \
		LinkLayerMessageFactory.h \
		NetworkLayerMessageFactory.h \
		TransportLayerMessageFactory.h \
		GenericLayerMessageFactory.h \
		Statistics.h \
		LayerMessageFactory.h \
		Option.h \
		PcapReader.h \
		Makefile \
		manual.pdf \
		README

all:
	$(CC) $(FLAGS) $(FILES)  -o analyzer

clean:
	rm -rf analyzer xtisov00.tar.gz

tar: tar
	tar -cf $(LOGIN).tar $(FILES) $(OTHERS)
