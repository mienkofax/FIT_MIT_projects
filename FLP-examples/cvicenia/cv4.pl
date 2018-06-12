% FLP CVICENI 4 - PROLOG 1 - UVOD

% ukazka predikatu pro vypocet funkce faktorial
factorial( 0, 1 ).
factorial( N, Value ) :-
     N > 0,
     Prev is N - 1,
     factorial( Prev, Prevfact ),
     Value is Prevfact * N.

% databaze rodinnych vztahu
muz(jan).
muz(pavel).
muz(robert).
muz(tomas).
muz(petr).

zena(marie).
zena(jana).
zena(linda).
zena(eva).

otec(tomas,jan).
otec(jan,robert).
otec(jan,jana).
otec(pavel,linda).
otec(pavel,eva).

matka(marie,robert).
matka(linda,jana).
matka(eva,petr).

% NOVE pridane
matka(linda,robert).

% Implementujte nasledujici predikaty:
% ----------------------------------------
% X je rodi Y pokud: X je otcem Y nebo X je matka Y
% PRIKLAD: 
% ?- rodic(X,jana).
% X = jan ;
% X = linda.
%
% ?- rodic(jan,jana).
% true .
% ?- rodic(linda,jana).
% true .
rodic(X,Y) :- otec(X,Y); matka(X,Y). 

% ----------------------------------------
% staci jen jeden spolecny rodic
% PRIKLAD:
% ?- sourozenec(robert,jana).
% true .
sourozenec(X,Y) :- rodic(Z,X), rodic(Z,Y). %, X\==Y. 

% maji oba rodice spolecne
% PRIKLAD:
% ?- sourozenecOba(robert,jana).
% false.
% PRIKLAD:
% pokud ale pridame fakt: matka(linda,robert).
% ?- sourozenecOba(robert,jana).
% true.
sourozenecOba(X,Y) :- matka(M,X), matka(M,Y), otec(O,X), otec(O,Y). 

% ----------------------------------------
% PRIKLAD:
% ?- sestra(linda,eva).
% true .
% PRIKLAD: pozor! pokud zmenime predikat na sourozenecOba
% potom dostaneme false! Protoze linda a eva maji pouze spolecneho otce (pavel)
sestra(X,Y) :- zena(X), sourozenec(X,Y).

% ----------------------------------------
% PRIKLAD:
% ?- deda(pavel,petr).
% true.
%
% ?- deda(tomas,X).
% X = robert ;
% X = jana ;
% false.
deda(X,Y) :- rodic(Z,Y), otec(X,Z).

% ----------------------------------------
% PRIKLAD:
% ?- je_matka(marie).
% true.
%
% ?- je_matka(jana).
% false.
je_matka(X) :- matka(X,_) % zena(X), rodic(X,_).

% ----------------------------------------
% PRIKLAD:
% ?- teta(eva,jana).
% true .
teta(X,Y) :- rodic(Z,Y), sestra(X,Z).


% Seznamy:
neprazdny([_|_]) :- true.
hlavicka([H|_], H).
posledni([H], H) :- !.
posledni([_|T], Res) :- posledni(T, Res).

% =======================================
% Dalsi ukoly:
% PRIKLAD:
% ?- spoj([2,1],[4,3],L).
% L = [2, 1, 4, 3].
% ?- trace, spoj([1,2],[3,4],L).
%   Call: (65) spoj([1, 2], [3, 4], _7558) ? creep
%   Call: (66) spoj([2], [3, 4], _7888) ? creep
%   Call: (67) spoj([], [3, 4], _7894) ? creep
%   Exit: (67) spoj([], [3, 4], [3, 4]) ? creep
%   Exit: (66) spoj([2], [3, 4], [2, 3, 4]) ? creep
%   Exit: (65) spoj([1, 2], [3, 4], [1, 2, 3, 4]) ? creep
% L = [1, 2, 3, 4].
%
% spojeni prazdneho seznamu a seznamu L je seznam L
spoj([],L,L).
% spojeni dvou neprazdnych seznamu: hlavicku prvniho seznamu vlozime do vysledneho seznamu 
% a vysledny seznam ziskame pomoci rekurze - spojujeme T a L
spoj([H|T],L,[H|TT]):- spoj(T,L,TT).

% ----------------------------------------
% PRIKLAD: 
% ?- obrat([1,2,3,4,5], L).
% L = [5, 4, 3, 2, 1].
% ?- trace, obrat([1,2], L).
%   Call: (57) obrat([1, 2], _7066) ? creep
%   Call: (58) obrat([2], _7396) ? creep
%   Call: (59) obrat([], _7396) ? creep
%   Exit: (59) obrat([], []) ? creep
%   Call: (59) spoj([], [2], _7404) ? creep
%   Exit: (59) spoj([], [2], [2]) ? creep
%   Exit: (58) obrat([2], [2]) ? creep
%   Call: (58) spoj([2], [1], _7066) ? creep
%   Call: (59) spoj([], [1], _7394) ? creep
%   Exit: (59) spoj([], [1], [1]) ? creep
%   Exit: (58) spoj([2], [1], [2, 1]) ? creep
%   Exit: (57) obrat([1, 2], [2, 1]) ? creep
% L = [2, 1].
%
% obraceni prazdneho seznamu je prazdny seznam
obrat([],[]).
% tim, ze volame predikat obrat obrat(T,R) se zanorime nakonec seznamu
% pote se provadi predikat spoj(R, [H], Res), ktery posledni ziskanou hlavicku pripoji na zacatek Res
% vynorujeme se z rekurze a opet: predposledni ziskanou hlavicku pripojime na zacatek Res, atd. 
obrat([H|T], Res) :- obrat(T,R), spoj(R, [H], Res).

% ----------------------------------------
% PRIKLAD:
% ?- trace, sluc([2,3],[1],L).
%   Call: (65) sluc([2, 3], [1], _7830) ? creep
%   Call: (66) 2@<1 ? creep
%   Fail: (66) 2@<1 ? creep
%   Redo: (65) sluc([2, 3], [1], _7830) ? creep
%   Call: (66) 2@>=1 ? creep
%   Exit: (66) 2@>=1 ? creep
%   Call: (66) sluc([2, 3], [], _8154) ? creep
%   Exit: (66) sluc([2, 3], [], [2, 3]) ? creep
%   Exit: (65) sluc([2, 3], [1], [1, 2, 3]) ? creep
% L = [1, 2, 3] .
%
% [7]  ?- sluc([3,4],[1,2],L).
% L = [1, 2, 3, 4] .
% pokud je nektery ze vstupnich parametru prazdnu seznam, vratime ho
sluc(L, [], L).
sluc([], L, L).

% pokud X<Y, volame rekurzivne sluc() bez teto hlavicky (XS), protoze nejmensi prvky se do seznamu vlozi nakonec
% pokud Y>=Y, volame rekurzivne sluc() bez teto hlavicky (YS) 
sluc([X|XS], [Y|YS], [X|T]):- X@<Y, sluc(XS,[Y|YS],T).
sluc([X|XS], [Y|YS], [Y|T]):- X@>=Y, sluc([X|XS],YS,T).

% ----------------------------------------
% PRIKLAD:
%  ?- trace, serad([1,2],L).
%   Call: (65) serad([1, 2], _7822) ? creep
%   Call: (66) serad([2], _8150) ? creep
%   Call: (67) serad([], _8150) ? creep
%   Exit: (67) serad([], []) ? creep
%   Call: (67) sluc([2], [], _8158) ? creep
%   Exit: (67) sluc([2], [], [2]) ? creep
%   Exit: (66) serad([2], [2]) ? creep
%   Call: (66) sluc([1], [2], _7822) ? creep
%   Call: (67) 1@<2 ? creep
%   Exit: (67) 1@<2 ? creep
%   Call: (67) sluc([], [2], _8148) ? creep
%   Exit: (67) sluc([], [2], [2]) ? creep
%   Exit: (66) sluc([1], [2], [1, 2]) ? creep
%   Exit: (65) serad([1, 2], [1, 2]) ? creep
%
% pomoci predikatu serad() se dostaneme na konec seznamu 
% potom se vola prdikat sluc(), ktery slouci seznamy ve spravnem poradi 
serad([], []).
serad([H|T],SL) :- serad(T, Pom), sluc([H], Pom, SL).

% ----------------------------------------
% spojení seznamu
% vykricnik nemusi byt
split([], [[]]) :- !.
% prazdny seznam do prvniho seznamu
% rozsekame T, dostaneme S
% pokud bychom chteli dalsi vysledek, tak by se mezera dosadila za H
% vykricnik, aby nepokracoval dale
split([' '|T], [[]|S]) :- split(T, S), !.
% vykricnik nemusi byt
% H pridame do prvniho seznamu
split([H|T], [[H|TS]|S]) :- split(T, [TS|S]), !.

% ----------------------------------------
plus(X,Y,Z) :- Z is X + Y.

zipWith(_, [], _, []).
zipWith(_, _, [], []).
zipWith(P, [H1|T1], [H2|T2], [H|T]) :-
	G =.. [P, H1, H2, H], % alternativa: plus(H1, H2, H)
	call(G),
	zipWith(P, T1, T2, T).
