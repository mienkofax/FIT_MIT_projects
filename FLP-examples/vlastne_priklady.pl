/*
Flatten seznamu - vytvorit predikat e,
ktery bere 2 argumenty. Prvni je seznam
libovolne zanorenych seznamu (i prazdnych),
napr. [[], [1, 2], [[[[]]],[atom, atom]]].
Druhy argument je vysledny seznam bez zanoreni.
*/

e([], []).
e([[]|R], Res) :- !, e(R, Res).
e([[X|XS]|YS], Res) :- !, e([X,XS|YS], Res).
e([V|XS], [V|Res]) :- e(XS, Res).

/*
Funkce XOR, ktera vraci symterickou diferenci
dvou mnozin (sjednoceni mnozin bez jejich
pruseciku). Bere prvni a druhy parametr
mnozinu reprezentovanou seznamem, treti
parametr je vysledna mnozina reprezentovana
seznamem.
*/
xor([], L, L) :- !.
xor(L, [], L) :- !.

% najdeme prvy vyskyt prvku a koniec
elem(X, [X|_]) :- !.
elem(X, [_|XS]) :- elem(X, XS).

% spojenie zoznamov
app([], L, L).
app([X|XS], L, [X|RS]) :- app(XS, L, RS).

sub([], _, []).
sub([X|XS], YS, RS) :- elem(X, YS), !, sub(XS, YS, RS).
sub([X|XS], YS, [X|RS]) :- sub(XS, YS, RS).
