##
# @file Makefile
# @Author Peter Tisovčík (xtisov00) <xtisov00@stud.fit.vutbr.cz>
# @date February, 2018
# @brief 18 Makefile pre kompilaciu projektu.
##

CC=g++
CFLAGS=-std=c++11 -Wall -pedantic
LIBS=-lm
LOGIN=xtisov00
ZIP_FILENAME=kko.proj3.${LOGIN}

all:
	$(CC) $(CFLAGS) $(LIBS) main.c gif2bmp.c -o gif2bmp

clean:
	rm -f *.o gif2bmp $(ZIP_FILENAME).zip

FILES= main.c \
	gif2bmp.c \
	gif2bmp.h \
	gif2bmp.pdf \
	Makefile

zip:
	/bin/rm -rf $(ZIP_FILENAME).zip $(ZIP_FILENAME)
	zip $(ZIP_FILENAME).zip $(FILES)
	unzip -d $(ZIP_FILENAME) $(ZIP_FILENAME)
	rm $(ZIP_FILENAME).zip
	zip -r $(ZIP_FILENAME).zip $(ZIP_FILENAME)
	/bin/rm -rf $(ZIP_FILENAME)
