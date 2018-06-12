% FLP - Prolog, cv. 5
% Klara necasova (xnecas24)
% Duben 2018


% 1. multimnozina (potencni mnozina) - mnozina vsech podmnozin dane mnoziny
% ===========================
% PRIKLAD:
% subbags([1,2],P).
% P = [[], [2], [1], [1, 2]] 
% POSTUP:
% pomoci subbags se zanorime na konec seznamu 
% pomoci addOneToAll pripojime dany prvek ke vsem ostatnim podmnozinam 
% pomoci append spojime 2. a 3. argument addOneToAll - tedy podmnozinu, 
% ke ktere jsme pridavali prvek a vyslednou podmnozinu z addOneToAll
% subbags([1, 2], _602)
% subbags([2], _916)
%
% subbags([], [[]])
% addOneToAll(2, [[]], [[2]])
% lists:append([[]], [[2]], [[], [2]])
%
% subbags([2], [[], [2]])
% addOneToAll(1, [[], [2]], [[1], [1, 2]])
% lists:append([[], [2]], [[1], [1, 2]], [[], [2], [1], [1, 2]])
% subbags([1, 2], [[], [2], [1], [1, 2]]) ? creep
% P = [[], [2], [1], [1, 2]]
subbags([], [[]]).
subbags([X|XS], P) :- subbags(XS, Tmp), addOneToAll(X, Tmp, Tmp2), append(Tmp, Tmp2, P).

% pridava prvek ke kazde mnozine
% addOneToAll(1, [[2,3],[3,4],[4,5]], R).
% R = [[1, 2, 3], [1, 3, 4], [1, 4, 5]] .
addOneToAll(_, [], []).
addOneToAll(E, [L|LS], [[E|L]|T]):- addOneToAll(E, LS, T).

% =================================
% dynamicke predikaty lze vkladat i odstranovat z databaze (nazev/arita)
:- dynamic robot/2, dira/1.

% vypis DB 
db(_) :- listing(robot(_,_)), listing(dira(_)).

% inicializace DB
%   1D pole, na pozicich mohou byt bud robot nebo dira nebo nic 
%   robot ma ID a pozici
%   dira ma jen pozici
% stav: 1 2 3 4 5 6
%       a o b c d x  
% 
% x = nic 
% a, b, c, d = roboti na danych pozicich
% o = diry      
init :- vytvor(a, 1), vytvor(2), vytvor(b, 3), vytvor(c, 4), vytvor(d, 5).

% smaze vse v DB
% delAll(_) :- retractall(robot(_,_)), retractall(dira(_)).

% DEMO:
% --------------------------
% 1) inicializce a vypis DB
% init.
% db(_).
% 
% robot(a, 1).
% robot(b, 3).
% robot(c, 4).
% robot(d, 5).
% dira(2).

% 2) robot "a" spadne do diry
% doprava(a).
% db(_).
%
% robot(b, 3).
% robot(c, 4).
% robot(d, 5).
% dira(2).

% 3) robot "c" se srazi s robotem "b" (oba zmizi)
% doleva(c).
% db(_).
%
% robot(d, 5).
% dira(2).

% 4) robot "d" se posune doprava
% doprava(d).
% db(_).
%
% robot(d, 6).
% dira(2).

% KONEC DEMA
% --------------------------


% 2a. tvorba a odstranovni robotu a der
% ===========================
% obsazeno - je na pozici robot nebo dira?
% PRIKLAD: 
% ?- obsazeno(3). 
% true. (na pozici 3 mame robota)
%
% ?- obsazeno(1).
% false. (na pozici 1 nic neni - ani robot, ani dira)
obsazeno(P) :- robot(_,P); dira(P).

% vytvori robota s danym indexem a pozici 
% PRIKLAD: viz vyse 
vytvor(I, P) :- not(obsazeno(P)), assert(robot(I,P)).

% vytvori diru na dane pozici
% PRIKLAD: viz vyse 
vytvor(P) :- not(obsazeno(P)), assert(dira(P)).

% odstrani cokoliv, co je na pozici
% PRIKLAD:
% odstranime robota na pozici 3, potom zkontroluje, zda je pozice skutecne volna 
% ?- odstran(3).
% true .
% ?- obsazeno(3).
% false.
odstran(P) :- retract(robot(_,P)); retract(dira(P)).

% 2b. obsazene pozice
% ===========================
% predikat BAGOF
% vzhodnocuje predikat a do seznamu vlozi unifikace, ktere se mu povedly
% Predikat bagof/3 nalezne všechny unifikace dane promenne/vzoru,
% ktere splni dany cil: bagof(Vzor, Cil, Bag): 
% Vzor – co chci unifikovat = promenne, ktere nas zajimaji 
% Cil – cil, pro který se unifikace hledaji = predikaty 
% Bag – vysledny seznam vsech navazani

% napr.:
% máme predikáty:
% a(1).
% a(2).
% a(3).
% bagof(X, a(X), B).
% X = [1,2,3]

% vrati seznam obsazenych pozic
% Vzor - zajimaji nas promenne P
% Cil - predikat obsazeno(P)
% Bag - vysledek ulozime do X
% PRIKLAD:
% ?- obsazene_pozice(X).
% X = [4, 5].   (protoze robota na pozici 3 jsme odstranili)
obsazene_pozice(X) :- bagof(P, obsazeno(P), X).

% vrati seznam pozic obsazenych roboty
% zapis  I^robot(I, P) zabranuje, aby se roboti delili podle I (id) - zapis pomoci strisky
% PRIKLAD:
% ?- obsazene_roboty(X).
% X = [4].
obsazene_roboty(X) :- bagof(P, I^robot(I, P), X).

% 2c. pohyb robotu
% ===========================
inkrementuj(X,Y) :- Y is X+1.
dekrementuj(X,Y) :- Y is X-1.

% pohnou doleva/doprava robotem s danym id
% PRIKLAD:
% v DB mame:
% vytvor(a,3).
% vytvor(b,4).
% vytvor(5). 
% 
% vypiseme si roboty v DB:
% ?- listing(robot(_,_)).
% :- dynamic robot/2.
%
% robot(b, 4).
% robot(a, 3).
% true.
% 
% vypiseme si klide i diry v DB:
% ?- listing(dira(_)).
% :- dynamic dira/1.
% dira(5).
% true.
%
% NYNI POHNEME ROBOTEM "a" DOLEVA
% ?- doleva(a).
% true. (pohyb se zdaril)
% 
% vypiseme si znovu roboty v DB:
% ?- listing(robot(_,_)).
% :- dynamic robot/2.
% robot(b, 4).
% robot(a, 2).
% true. (ROBOT se posunul z pozice 3 doleva na pozici 2!!!! OK!)

doleva(I) :- pohni(I, dekrementuj).
doprava(I) :- pohni(I, inkrementuj).

% POZNAMKA: odstranovani vsech predikatu z DB:
% retractall(dira(_)).
% retractall(robot(_,_)).
% kdyz potom vypiseme obsah DB, tak se vypise jen true a to je vse -> tzn. vsechny predikaty byly odstraneny

% -----------------------
% poznamka: misto: G =..[Operace,P,N], call(G) by slo napsat i call(Operace,P,N)
% retract(robot(I,P)): odstranime robota s danym id a na dane pozici z databaze
% G =..[Operace,P,N], call(G): dynamicky vytvorime predikat
% Ruzne situace:
% a) je volno - robot se pohne (doleva/doprava) -> vytvorime tedy robota s indexem I na dane pozici N
% b) neni volno - pokud robot narazi na diru - "spadne" do ni - tedy robot zmizi!!! 
% c) neni volno - pokud robot narazi na jineho robota - srazi se --> oba zmizi ze sveta!!!
%
% logiku delaji znaky "->" a strednik! If Else nejsou klicova slova!!!
% Semantika:
% if (obsazeno(N ) 
%     // pokud je to dira, chceme ji nechat v DB! Musime zamezit tomu, aby byla odstranena predikatem odstran(N)
%     // takze JEN vratime true
%     if (dira(N )) 
%        true; 
%     // na pozici je obsazeno a NENI to dira (je to robot) - odstranime ho   
%     else      
%        odstran(N ); 
% // pozice neni obsazena - vytvorime robota
% else 
%    vytvor(I, N ); 
   
pohni(I, Operace) :- retract(robot(I,P)), G =..[Operace,P,N], call(G), (obsazeno(N) -> (dira(N) -> true; odstran(N));vytvor(I,N)).

% Poznamka: pokud by to bylo takhle, tak by se odstranovaly i diry!!!
% pohni(I, Operace) :- retract(robot(I,P)), G =..[Operace,P,N], call(G), ((obsazeno(N) -> odstran(N)) ; vytvor(I,N)).



% 2d. armageddon - vybuch vsech robotu, zustanou na jejich miste diry
% ===========================
% forall(podminka,akce)
% pro vsechny roboty se provede akce vybuch, u robota nezalezi, na ktere pozici se nachazi (proto je tu znak podtrzitko)
armageddon :- forall(robot(_,P), vybuch(P)).
% odstranime robota a misto nej vytvorime diru
vybuch(P) :- odstran(P), vytvor(P).


% 3. odemykaci gesta
% ===========================
g_size(3).

% Je Z v rozsahu 1-S?
% overuje, zda je souradnice platna (v rozsahu 1:1-S:S,
% kde S je pocet radku/sloupcu ziskany pomoci g_size).
% g_ok(Z) :- integer(Z), g_size(S), Z >= 1, Z =< S.
% g_test(X:Y) :- g_ok(X), g_ok(Y).
% g_test(1:3).
% true.
% g_test(1:4).
% false.
g_test(X:Y) :- g_size(S), between(1, S, X), between(1, S, Y).

g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 + 0, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 + 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 0, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 0, Y2 is Y1 + 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 + 0, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 + 1, g_test(X2:Y2).

%g_one(2:2, 3, [], Res).
%Res = [2:2, 1:1, 1:2] ;
%Res = [2:2, 1:1, 2:1] ;
%Res = [2:2, 1:2, 1:1] ;
%Res = [2:2, 1:2, 1:3] ;
%Res = [2:2, 1:2, 2:1] ;
%Res = [2:2, 1:2, 2:3] ;
%Res = [2:2, 1:3, 1:2] ;
%Res = [2:2, 1:3, 2:3] ;
%Res = [2:2, 2:1, 1:1] ;
%Res = [2:2, 2:1, 1:2] ;
%Res = [2:2, 2:1, 3:1] ;
%Res = [2:2, 2:1, 3:2] ;
%Res = [2:2, 2:3, 1:2] ;
%Res = [2:2, 2:3, 1:3] ;
%Res = [2:2, 2:3, 3:2] ;
%Res = [2:2, 2:3, 3:3] ;
%Res = [2:2, 3:1, 2:1] ;
%Res = [2:2, 3:1, 3:2] ;
%Res = [2:2, 3:2, 2:1] ;
%Res = [2:2, 3:2, 2:3] ;
%Res = [2:2, 3:2, 3:1] ;
%Res = [2:2, 3:2, 3:3] ;
%Res = [2:2, 3:3, 2:3] ;
%Res = [2:2, 3:3, 3:2] ;
%false.
% g_one(X1:Y1, 0, _, [X1:Y1]).
% g_one(X1:Y1, Len, L, [X1:Y1|Res]) :- g_move(X1:Y1, X2:Y2), not(member(X2:Y2, L)), Len1 is Len-1, g_one(X2:Y2, Len1, [X1:Y1|L], Res).
g_one(P, 1, _, [P]) :- !.
g_one(P, Len, L, [P|Res]) :- g_move(P, P2), \+ member(P2, L), Len2 is Len - 1, g_one(P2, Len2, [P|L], Res).

%g_all(Res,2).
%Res = [1:1, 1:2] ;
%Res = [1:1, 2:1] ;
%Res = [1:1, 2:2] ;
%Res = [1:2, 1:1] ;
%Res = [1:2, 1:3] ;
%Res = [1:2, 2:1] ;
%Res = [1:2, 2:2] ;
%Res = [1:2, 2:3] ;
%Res = [1:3, 1:2] ;
%Res = [1:3, 2:2] ;
%Res = [1:3, 2:3] ;
%Res = [2:1, 1:1] ;
%Res = [2:1, 1:2] ;
%Res = [2:1, 2:2] ;
%Res = [2:1, 3:1] ;
%Res = [2:1, 3:2] ;
%Res = [2:2, 1:1] ;
%Res = [2:2, 1:2] ;
%Res = [2:2, 1:3] ;
%Res = [2:2, 2:1] ;
%Res = [2:2, 2:3] ;
%Res = [2:2, 3:1] ;
%Res = [2:2, 3:2] ;
%Res = [2:2, 3:3] ;
%Res = [2:3, 1:2] ;
%Res = [2:3, 1:3] ;
%Res = [2:3, 2:2] ;
%Res = [2:3, 3:2] ;
%Res = [2:3, 3:3] ;
%Res = [3:1, 2:1] ;
%Res = [3:1, 2:2] ;
%Res = [3:1, 3:2] ;
%Res = [3:2, 2:1] ;
%Res = [3:2, 2:2] ;
%Res = [3:2, 2:3] ;
%Res = [3:2, 3:1] ;
%Res = [3:2, 3:3] ;
%Res = [3:3, 2:2] ;
%Res = [3:3, 2:3] ;
%Res = [3:3, 3:2] ;
%false.
g_all(R, Len) :- g_test(Start), g_one(Start, Len, [], R).

%g_allLength(R).
g_allLength(R) :- g_size(S), MaxLen is S*S, between(1, MaxLen, Len), g_all(R, Len).


