#!/bin/bash
rm -f output.xml
rm -f delta.xml
rm -f file
rm -f testy2.zip
rm -f xtisov00-CLS.zip

rm -r testdir
zip -r testy2.zip testy-cls/ test.sh doc/ cls_options jexamxml.jar samples/
zip xtisov00-CLS.zip rozsireni cls.php
./is_it_ok.sh xtisov00-CLS.zip testdir
