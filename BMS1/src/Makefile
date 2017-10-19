CC=g++
FLAGS= -std=c++11 -Wall -pedantic -Wextra
LOGIN=xtisov00
FILES=  Application.cpp \
		BTSInfo.cpp \
		CSVParser.cpp \
		GPSCoordinate.cpp \
		HataModel.cpp \
		Location.cpp \
		UTMWrapper.cpp \
		main.cpp

OTHERS= Application.h \
		BTSInfo.h \
		CSVParser.h \
		GPSCoordinate.h \
		HataModel.h \
		Location.h \
		UTM.h \
		UTMWrapper.h \
		Makefile

all:
	$(CC) $(FLAGS) $(FILES) -o p1 -L/usr/local/lib -L/usr/local/lib64 -lPocoFoundation -lPocoUtil

clean:
	/bin/rm -rf p1 $(LOGIN).zip

zip:
	zip $(LOGIN).zip $(FILES) $(OTHERS)
