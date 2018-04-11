#!/bin/bash

# Soubor:  test_time.sh
# Datum:   duben 2018
# Autor:   Klara Necasova
# Email:   xnecas24@stud.fit.vutbr.cz
# Projekt: Implementace algoritmu "Merge-splitting sort" - projekt c. 2 pro predmet PRL


numberProc=4
#runs=10

# compiles
mpic++ --prefix /usr/local/share/OpenMPI -o mss mss.cpp -std=c++11

# theoretical time complexity
# sequence range: 100 000 - 1 000 000 
# 10 runs for each sequence, then average
for w in {1..10}0000 
do
   dd if=/dev/urandom bs=1 count=$w of=numbers 2>/dev/null 
   echo "TEST FOR SEQUENCE LENGTH=$w"
   for x in {1..10}
   do
    #/usr/bin/time -f "%U" -o app_time 
    mpirun --prefix /usr/local/share/OpenMPI -np $numberProc mss
    #current_time=$(cat app_time)
    #total_time=$(echo $total_time+$current_time | bc)      
   done
   #total_time=$(echo "$total_time/$runs" | bc -l)
   #echo "$total_time"
done

# removes created files
rm -f mss numbers				
