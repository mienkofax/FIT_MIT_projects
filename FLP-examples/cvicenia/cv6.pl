% PROLOG cv. 6
% Klara Necasova (xnecas24)

:- dynamic velikost/2, poz/2, slovnik/3.

% 1. Rozdil mnozin
% ================================
% zjisti, zda je H prvkem seznamu
prvek(H, [H|_]) :- !.
prvek(H, [_|T]) :- prvek(H,T).


% rozdil mnozin
% PRIKLAD:
% ?- rozdil([1,2,3,4], [2,4], L).
% L = [1, 3].
rozdil([], _, []).
% pokud H je prvkem S, potom prvek H nebude zahrnut ve vysledku
% -> rekurzivne volame predikat rozdil() a pokracujeme
rozdil([H|T], S, R) :- prvek(H,S), !, rozdil(T, S, R).

% pokud H neni prvkem S, potom ho nemuzeme smazat a zahrneme
% ho tedy do vysledku
% -> rekurzivne volame predikat rozdil() a pokracujeme
rozdil([H|T], S, [H|P]) :- rozdil(T,S,P).


% 2. Problem dam (N-Queens Problem)
% ================================
% REPREZENTACE:
% 1D pole celych cisel
% indexy pole udavaji radky
% hodnoty v poli udavaji sloupce
% Priklad:
% [4, 2, 1, 3]
% -> 4 damy, pozice: (1,4), (2,2), (3,1), (4,3)
% pozn: pro zobrazeni vsech reseni musime mackat ";"

% sequence - vygeneruje seznam o delce N
% PRIKLAD:
% sequence(4, N).
% N = [4, 3, 2, 1].
sequence(0, []) :- !.
sequence(N, [N|T]) :- X is N-1, sequence(X,T).

% test - ohrozuji se damy?
% musime kazdou damu porovnat vuci zbytku
% jakmile nemame damy, tak koncime
% pokud je seznam prazdny - konec
test([]) :- !.
% test(dama, vzdalesnost, zbytek)
% H - 1. dama, T - ostatní damy
test([H|T]) :- test(H,1,T), test(T).
% ukoncujici podminka - seznam je prazdny
test(_, _, []) :- !.

% HLAVNI TESTOVANI:
% Pos - pozice damy (index sloupce)
% Dist - vzdalenost
% [H|T] - seznam s pozicemi ostatnich dam
%
% POZOR! co je potreba tedy testovat?
% damy nesmi byt ve stejnem radku - ok
% -> (1,1), (1,2)... - nikdy nenastane, protoze indexy radku se automaticky zvysuji (jsou dany indexy v poli dam)
% damy nesmi byt ve stejnem sloupci - ok
% -> (1,1), (2,1)... - nikdy nenastane, opet diky kreprezentaci problemu
%
% TEDY: staci testovat jen DIAGONALY:
% hlavni diagonala: (1,1), (2,2)...
% vedlejsi diagonaly (1,2), (2,3)...
% idea: vypocteme absolutni hodnotu Y-souradnic
% a podivame se, jestli se vysledek nerovna testovane vzdalenosti (Dist)
% pokud ano - damy se ohrozuji, pokud ne - vse je ok
% zvysime vzdalenost Dist a pokracujeme
% Pos - y-souradnice
% [H|T] - 1. dama ze zbytku
% Dist na zacatku 1
test(Pos, Dist, [H|T]) :-
    Tmp is abs(Pos - H),
    Tmp =\= Dist,
    DistNew is Dist + 1,
    test(Pos, DistNew, T).

% pokud zadame pouze: queens(L).
% implicitne predpokladame velikost sachovnice 8x8
% pokud chceme jiny rozmer sachovnice, musime zavolat  predikat nize:
% napr. queens(4, L).
queens(Solution) :- queens(8, Solution).

% N - pocet dam
% Solution - reseni
% vygenerujeme 1D pole dam
% promichame hodnoty v poli (permutation)
% otestujeme, zda ma uloha reseni
%
% PRIKLAD:
% ?- queens(4, L).
% L = [3, 1, 4, 2] ;
% L = [2, 4, 1, 3] ;
% false.
queens(N, Solution) :-
	sequence(N, List),
	permutation(List, Solution),
	test(Solution).
%test(Solution) :- true. % zatím


% 3. Cesty kone
% ================================

% test, zda jsou souradnice v rozsahu
testPoz(X,Y) :- velikost(XR, YR),
	X > 0, Y > 0,
	X =< XR, Y =< YR.

%testPoz(_,_). %dummy - comment it out!

% definice skoku kone (ruzna "L")
skok(X,Y,XN,YN) :- XN is X + 2, YN is Y + 1, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X + 2, YN is Y - 1, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X - 2, YN is Y + 1, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X - 2, YN is Y - 1, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X + 1, YN is Y + 2, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X + 1, YN is Y - 2, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X - 1, YN is Y + 2, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X - 1, YN is Y - 2, testPoz(XN, YN).

% 1 cesta
% -----------------
% odkud: X, Y
% kam: X, Y
% pokud se z nejake pozice chceme dostat na tu stejnou - ok
% cesta(X,Y,X,Y,[X:Y]).

% cesta(X,Y,X,Y,[X:Y]) :- !, fail.

% TODOS
cesta(X,Y,X,Y,[X:Y]) :- !.

% odkud: X, Y
% kam: XE, YE
% skok() - provedeme skok na pozici XN, YN
% not poz(XN,YN) - podivame se, zda jsme na dane pozici jiz nebyli
% assert(poz(X,Y)) - pokud ne, vlozime pozici do DB
% cesta(...) - hledame nove cesty z nove pozice XN, YN
cesta(X,Y,XE,YE,[X:Y|T]) :- skok(X, Y, XN, YN), not(poz(XN, YN)),
	assert(poz(X,Y)), cesta(XN, YN, XE, YE, T).

% musime oznacit pozici XY jakoze jsme na ni nebyli
% abychom ji mohli potom zahrnout do dalsich vysledku
% v priprade hledani vice cest
cesta(X, Y, _, _, _) :- retract(poz(X,Y)), !, fail.

% nalezeni vsech cesta kone
% --------------------------------
% mame predikat cesta() - vraci cestu mezi 2 misty
% potrebujeme znat vsechny cesta - potrebujeme volat predikat opakovane!
% assert(velikost(XR, YR)) - vlozime do DB velikost sachovnice
% findall(...) - najdeme vsechny mozne cesty z pozice XS,YS do pozice XE,YE
% length(L, N) - zjistime, kolik reseni jsme nasli
% nakonec odstranime navstivene pozice a velikost sachovnice z DB
%
% PRIKLAD:
% ?- cesty(3,3,1,1,1,3,N).
% N = 2.
 cesty(XR, YR, XS, YS, XE, YE, N) :- assert(velikost(XR, YR)),
	 findall(C, cesta(XS, YS, XE, YE, C), L),
	 length(L, N), retractall(poz(_,_)), retractall(velikost(_,_)).

% pokud bychom chteli vypsat si pozice, musime predikat upravit, protoze na konci se pozice mazou
% musime zakomentivat predikat cesty() vyse
% po spusteni dame: listing(poz(_,_)).
% PRIKLAD:
% ?- cesty(3,3,1,1,1,3,N).
% N = [[1:1, 3:2, 1:3], [1:1, 2:3, 3:1, 1:2, 3:3, 2:1, 1:3]]
% cesty(XR, YR, XS, YS, XE, YE, L) :- assert(velikost(XR, YR)),
	% findall(C, cesta(XS, YS, XE, YE, C), L).




% 4. Asociativni pamet
% ================================
% init DB
init :- slovnik(d,'a',1), slovnik(d,'b',1), slovnik(d,'c',2), slovnik(d,'d',3).
% vypis DB
print :- listing(d(_,_)).
% smazani DB
del :- retractall(d(_,_)).

% PRIKLAD:
% na zacatku:
% %- init.
% ?- print.
% d(a, 1).
% d(b, 1).
% d(c, 2).
% d(d, 3).


% kontroly
slovnik(D, _, _) :- var(D), !, fail.
slovnik(_, K, V) :- var(K), var(V),!, fail.

% vyhledani hodnoty
% G=..[D,K,V], call(G). NEBO call(D, K, V).
% PRIKLAD:
% ?- slovnik(d,'a',V).
% V = 1.
slovnik(D, K, V) :- var(V), !, call(D, K, V).

% vyhledani klicu
% overeni, ze var(K) je volna promenna
% nyni hledame klice - potrebujeme se opakovane dotazovat DB
% jedna hodnota, vice klicu
% PRIKLAD:
% ?- slovnik(d,K,1).
% K = [a, b].
slovnik(D, K, V) :- var(K), !, G=..[D,Kx,V], bagof(Kx,G,K).

% modifikace
% zjistime polozku s danym klicem (0=..) a smazeme ji
% vlozime novou polozku (N=..)
% PRIKLAD: menime hodnotz polozky s klicem 'a'
% slovnik(d,'a',10). // puvodne: (d,'a',1).
% print.
% ...
% d(a, 10).
slovnik(D, K, V) :-
	% toto bylo na slidech, odkomentovane reseni je moje, podle toho, co rikal, tak to nejak asi backtrackuje, i kdyby nemelo
	%O =.. [D,K,_], call(O),!, retract(O),
    O =.. [D,K,_], retract(O),
	N =..[D,K,V], assert(N).

% vlozeni
slovnik(D, K, V) :- N =..[D,K,V], assert(N).
