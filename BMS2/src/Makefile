CC=g++
CFLAGS=-std=c++11 -Wall -pedantic -O2
LIBS=-lm
LOGIN=xtisov00

all: bms2A bms2B

bms2A: bms2A.cpp ModulatorException.cpp AbstractModulator.cpp Modulator.cpp
	$(CC) $(CFLAGS) $(LIBS) $^ -o $@ libsndfile.a

bms2B: bms2B.cpp ModulatorException.cpp AbstractModulator.cpp Demodulator.cpp
	$(CC) $(CFLAGS) $(LIBS) $^ -o $@ libsndfile.a

clean:
	rm -f *.o bms2A bms2B

FILES= bms2A.cpp \
	bms2B.cpp \
	ModulatorException.h \
	ModulatorException.cpp \
	AbstractModulator.h \
	AbstractModulator.cpp \
	Modulator.h \
	Modulator.cpp \
	Makefile

zip:
	zip $(LOGIN).zip $(FILES)

