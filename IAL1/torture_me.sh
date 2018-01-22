#!/bin/sh
#
#předem bych chtěl podotknout, že tyto testy byly přebrány z FIT serveru a 
#fitušky z loňských let a proto dík patří někomu jinému než mě :) já jsem je 
#jenom složil dohromady. Taky bych chtěl říct, že nepřebírám žádnou odpovědnost
#za správné výsledky ani málo bodů ve WISu.
#
#
#POKUD NAJDETE NĚJAKÉ BUGY NEBO UDĚLÁTE LEPŠÍ TESTY, TAK MI NAPIŠTE - BUDOU SE HODIT :)
#
#
#sada testů pro první domácí úkol do IALu
#soubor torture_me.sh musí být umístěn v adresáři, který obsahuje adresáře
#c201, c203, c206, které obsahují testovací soubory, správné výstupy, soubory  .c a .h
#a soubor Makefile
#
#
#skript postupně projede adresáře, pomocí make vytvoří spustitelné programy,
#programy spustí a jejich výstupy uloží do souborů .txt
#výstupy porovná s refenčními a vytvoří jejich rozdíl, který uloží do 
#příslušného souboru diff*.txt (pokud je soubor nulové velikosti, program 
#funguje správně)
#
#při správném výstupu se také provede ten na memory leaks valgrindem a jeho 
#výsledek se vypíše do souboru valgrind.txt a na terminál
#pokud není výstup správný, nakonec vám program nabídne rozdíly souborů vypsat 
#na terminál
#
#
# 

for t in c201 c203 c206
do
  echo "################################################################################"
  echo "Running tests for "$t":"                                                       
  cd $t
    test1=$t-aastandard-test
    test2=$t-advanced-test
    rm $test1 $test2 2>/dev/null >/dev/null
    if [ "$t" = "c206" ]
    then
      test3=$t-advanced-test2
      rm $test3 2>/dev/null >/dev/null
    fi
    make
    chmod +x $test1
    chmod +x $test2
################### standard test #############################################
    echo -n "   standard test:"
    ./$test1 > $test1-result.txt
    diff $test1.output $test1-result.txt >diff-standard.txt
    if [ -s diff-standard.txt ]
      then  echo "      failed"
            echo "Do you want to check differences (Y/N)?"
            read x
            if [ "$x" = "Y" -o "$x" = "y" ]
              then
                cat diff-standard.txt
            fi
      else echo "      success"
          valgrind --log-file=valgrind.txt ./$test1 >/dev/null
          echo "     ---------------------------------------------------------------VALGRIND OUT"
          cat valgrind.txt | sed 's/^.*[=]/     |/g' | grep bytes 
          echo "     ---------------------------------------------------------------VALGRIND OUT"
    fi
################### advanced test #############################################
    echo -n "   advanced test:"
    ./$test2 > $test2-result.txt
    diff $test2.output $test2-result.txt >diff-advanced.txt
    if [ -s diff-advanced.txt ]
      then  echo "      failed"
            echo "Do you want to check differences (Y/N)?"
            read x
            if [ "$x" = "Y" -o "$x" = "y" ]
              then
                cat diff-advanced.txt
            fi
      else echo "      success"
          valgrind --log-file=valgrind.txt ./$test2 >/dev/null
          echo "     ---------------------------------------------------------------VALGRIND OUT"
          cat valgrind.txt | sed 's/^.*[=]/     |/g' | grep bytes 
          echo "     ---------------------------------------------------------------VALGRIND OUT"
    fi
################### advanced test2 only in DL list#############################    
    if [ "$t" = "c206" ]
    then
      echo -n "   advanced test 2:"
      ./$test3 > $test3-result.txt
      diff $test3.output $test3-result.txt >diff-advanced2.txt
      if [ -s diff-advanced2.txt ]
        then  echo "      failed"
              echo "Do you want to check differences (Y/N)?"
              read x
              if [ "$x" = "Y" -o "$x" = "y" ]
                then
                  cat diff-advanced2.txt
              fi
        else echo "    success"
          valgrind --log-file=valgrind.txt ./$test3 >/dev/null
          echo "     ---------------------------------------------------------------VALGRIND OUT"
          cat valgrind.txt | sed 's/^.*[=]/     |/g' | grep bytes 
          echo "     ---------------------------------------------------------------VALGRIND OUT"
      fi  
    fi
    
  cd ..
  echo ""
done
echo "Do you want to remove temporary files (Y/N)?"
read x
if [ "$x" = "Y" -o "$x" = "y" ]
  then
    rm -f c201/*.txt c203/*.txt c206/*.txt
    rm -f c201/c201-aastandard-test c201/c201-advanced-test
    rm -f c203/c203-aastandard-test c203/c203-advanced-test
    rm -f c206/c206-aastandard-test c206/c206-advanced-test c206/c206-advanced-test2
fi
############################## END ############################################
