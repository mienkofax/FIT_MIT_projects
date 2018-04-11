# simplify-bkg
# Peter Tisovčík (xtisov00)

LOGIN=xtisov00
PROJECT=simplify-bkg
PROJECT_TYPE=fun
SUBJECT=flp

all:
	ghc --make -o ${PROJECT} ${PROJECT}.hs

clean:
	/bin/rm -rf *.hi *.o

cleanall:
	/bin/rm -rf *.hi *.o ${PROJECT} ${SUBJECT}-${PROJECT_TYPE}-${LOGIN}.zip

zip:
	zip -r ${SUBJECT}-${PROJECT_TYPE}-${LOGIN}.zip Makefile ${PROJECT}.hs dir_in dir_ref readme.md readme-tests.md it_is_ok.sh run_tests.sh

