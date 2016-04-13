#!/bin/bash
rm -f output.xml
rm -f delta.xml
rm -f file
rm -f testy2.zip
rm -f xtisov00-CSV.zip
rm f testy2.zip

rm -r testdir
zip xtisov00-CSV.zip csv.py
./is_it_ok.sh xtisov00-CSV.zip testdir
