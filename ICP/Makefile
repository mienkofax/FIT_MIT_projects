#
# @file			Makefile
# @author		Klára Nečasová <xnecas24>
# @author		Peter Tisovčík <xtisov00>
#

CXX=g++
CXXFLAGS=-Wall -pedantic -std=c++11 -lm
CLI_DIR=src/cli
GUI_LIB=src/qt
CORE_DIR=src/core
LIBRARY_OTHELLO=$(CORE_DIR)/othello-lib.a

NAME_CLI=hra2016-cli
NAME_GUI=hra2016

all: library hra2016-cli hra2016

#vytvorenie vsetkeho
run: library hra2016-cli hra2016 doxygen

#vytvorenie knizcnice
library:
	make -C $(CORE_DIR)

#vytvorenie cli hry
hra2016-cli: library
	g++ $(CLI_DIR)/main.cpp $(CLI_DIR)/cli.cpp $(LIBRARY_OTHELLO) -o $(NAME_CLI) $(CXXFLAGS) -I$(CLI_DIR) -I$(CORE_DIR) -L$(CORE_DIR)

#vytvorenie gui hry
hra2016: library
	qmake $(GUI_LIB)/qt-gui.pro -o $(GUI_LIB)/Makefile
	make -C $(GUI_LIB)
	mv $(GUI_LIB)/$(NAME_GUI) $(NAME_GUI)

#vytvorenie archivu
pack:
	zip xtisov00-xnecas24.zip -r Makefile READMI.txt src/cli src/core src/qt src/doxygen.conf examples

#vytvorenie dokumentacie
doxygen:
	rm -rf doc
	mkdir doc
	doxygen src/doxygen.conf

#odstranenie nepotrebnych veci
clean:
	make -C $(CORE_DIR) cleanall
	rm -f $(GUI_LIB)/*.o $(GUI_LIB)/qt-gui.pro.user $(GUI_LIB)/ui_gui.h $(GUI_LIB)/Makefile
	rm -f $(GUI_LIB)/qt-gui.pro.user $(GUI_LIB)/moc_*.cpp
	rm -f $(NAME_CLI) $(NAME_GUI) xtisov00-xnecas24.zip
	rm -rf doc
