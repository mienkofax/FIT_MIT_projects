% Rezy v Prologu
% databaze faktu 
% zdroj: http://www.cse.unsw.edu.au/~billw/dictionaries/prolog/cut.html

% teaches(profesor, predmet)
teaches(dr_fred, history).
teaches(dr_fred, english).
teaches(dr_fred, drama).
teaches(dr_fiona, physics). 	          	

% studies(student, predmet)
studies(alice, english).
studies(angus, english).
studies(amelia, drama).
studies(alex, physics).

% 1. PRIKLAD: zadne rezy
% a) zkousi se Course = history, hledame studenry historie - zadni nejsou -> cil selze
% b) zkousi se Course = english, hledame studenty -> alice, angus 
% c) zkousi se Course = drama, hledame studenty -> amelia 
% 
% ?- teaches(dr_fred, Course), studies(Student, Course).

% Course = english
% Student = alice ;

% Course = english
% Student = angus ;

% Course = drama
% Student = amelia ;

% false.

% =====================
% 2. PRIKLAD: rez uprostred
% a) zkousi se Course = history, hledame studenty historie - zadni nejsou -> cil selze
% nyni naratime na rez (!) a ten nam zabrani abychom zkouseli znovu volat predikat teaches() a zkouseli jine moznosti 
% cely pokus selze - vrati se false. 
%
% ?- teaches(dr_fred, Course), !, studies(Student, Course).
% false.

% ======================
% 3. PRIKLAD: rez na konci
% a) zkousi se Course = history, hledame studenry historie - zadni nejsou -> cil selze (nenarazili jsme ještě na řez na konci - ok) 
% -> backtracking se provede
% b) zkousi se Course = english, hledame studenty -> jako prvni se najde alice 
% nyni narazime na rez! Cil uspel a zadna dalsi reseni se nehledaji - najde se tedy pouze 1 reseni!  
%
% ?- teaches(dr_fred, Course), studies(Student, Course), !.
% Course = english
% Student = alice ;
% false.

% ======================
% 4. PRIKLAD: rez na zacatku
% vysledek je stejny jako v prikladu 1 -> zane rezy 
% ?- !, teaches(dr_fred, Course), studies(Student, Course).

% ======================
% ======================
% 5. PRIKLAD: rezy v pravidlech 
% pokud je spoleno prvni pravidlo, pouzijeme ho a uz se nezabyvame druhym pravidlem 
% -> tzn. pokud X > Y, tak se proste vrati X a dal nic neresim (je to jedina pravna odpoved) 
% max1(X, Y, X) :- X > Y, !.
% max1(_X, Y, Y).
%
% Ekvivalentni zapis: muze by mene efektivni, kdyby se v tele predikatu provadelo neco slozitejsiho 
% max2(X, Y, X) :- X > Y.
% max2(X, Y, Y) :- X =< Y. 
% 
% Take lze napsat: ale nepouziva se to. Kdyz uz mame ten rez, tak druhe pravidlo by melo byt jen max3(_X, Y, Y).
% max3(X, Y, X) :- X > Y, !.
% max3(X, Y, Y) :- X =< Y.


