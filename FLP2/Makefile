# Rubikova kocka
# Peter Tisovcik (xtisov00)

LOGIN=xtisov00
PROJ=flp18-log
ARCHIVEPROJ=flp-log-$(LOGIN)
FILES=Makefile rubik.pl test1.in test2.in test3.in test4.in test5.in README

all:
	swipl -q -g main -o $(PROJ) -c rubik.pl

zip: all
	zip -r $(ARCHIVEPROJ).zip $(FILES)

clean:
	rm -f $(PROJ)
