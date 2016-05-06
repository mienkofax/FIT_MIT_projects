CXX=g++
CXXFLAGS=-Wall -pedantic -std=c++11 -lm
CLI_DIR=src/cli
GUI_LIB=src/qt
CORE_DIR=src/core
LIBRARY_OTHELLO=$(CORE_DIR)/othello-lib.a

NAME_CLI=hra2016-cli
NAME_GUI=hra2016

all: library hra2016-cli hra2016

run: library hra2016-cli hra2016 doxygen

library:
	make -C $(CORE_DIR) library

hra2016-cli: library
	g++ $(CLI_DIR)/main.cpp $(CLI_DIR)/cli.cpp $(LIBRARY_OTHELLO) -o $(NAME_CLI) $(CXXFLAGS) -I$(CLI_DIR) -I$(CORE_DIR) -L$(CORE_DIR)

hra2016: library
	export PATH=/usr/local/share/Qt-5.5.1/5.5/gcc_64/bin/:$PATH
	export LD_LIBRARY_PATH=/usr/local/share/Qt-5.5.1/5.5/gcc_64/lib
	qmake $(GUI_LIB)/qt-gui.pro -o $(GUI_LIB)/Makefile
	make -C $(GUI_LIB)
	cp $(GUI_LIB)/$(NAME_GUI) $(NAME_GUI)

pack:
	zip xnecas24-xtisov00.zip -r Makefile src/cli src/core src/qt doxygen.conf

doxygen:
	mkdir doc
	doxygen src/doxygen.conf

clean:
	make -C $(CORE_DIR) cleanall
	rm -f $(GUI_LIB)/*.o
	rm -f $(GUI_LIB)/Makefile
	rm -f $(GUI_LIB)/$(NAME_GUI)
	rm -f $(NAME_CLI)
	rm -f $(NAME_GUI)
	rm -rf doc
	rm -f xnecas24-xtisov00.zip
