% X je prvkom zoznamu
prvek(X, [X|_]).
prvek(X, [_|T]) :- prvek(X, T).

% zoznam L vznikne vypustenim prvku X zo zoznamu S
vypust(X, [X|T], T).
vypust(X, [Y|T], [Y|L]) :- vypust(X, T, L).

% zoznam L vznikne vypustenim vsetkych prvkov X zo zoznamu S
vypustVsechny(X, [X|T], L) :- vypustVsechny(X, T, L).
vypustVsechny(X, [Y|T], [Y|L]) :- vypustVsechny(X, T, L).
vypustVsechny(X, [], []).

% posledny prvok
posledny([X], X).
posledny([X|T], Y) :- posledny(T, Y).

% zisti posledny prvok a vrati zoznam bez posledneho prvku
poslednyZbytok([X], [], X).
poslednyZbytok([X|T], [X|T1], Y) :- poslednyZbytok(T, T1, Y).

% prvy prvok zoznamu
prvy([X|_], X).

% zisti prvy prvok a vrati zoznam bez prveho prvku
prvyZbytok([X|T], T, X).

% prostredny prvok zoznamu
prostredny([U], [X|T], X).
prostredny([U, V], [X|T], X).
prostredny([U, V|T1], [W|T2], X) :- prostredny(T1, T2, X).

% rozdelenie zoznamu na dva zoznamy, jeden je zoznam s lichymi
% indexami, druhy s parnymi
rozdel([X, Y|T], [X|T1], [Y|T2]) :- rozdel(T, T1, T2).
rozdel([X], [X], []).
rozdel ([], [], []).

% zoznam L vznikne zretazenim zoznamu L1 a L2
cons([], L, L).
conc([X|T], L, [X|S]) :- conc(T, L, S).

% obratenie zoznamu
obrat([], []).
obrat([X|T])
