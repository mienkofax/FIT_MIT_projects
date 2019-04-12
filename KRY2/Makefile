SRC="eliptic_curve.py"

all: decipher

decipher:
		@python3 $(SRC) "${publicKey}"

test:
		./test.sh

tgz:
		tar -cvzf xtisov00.tgz Makefile eliptic_curve.py
