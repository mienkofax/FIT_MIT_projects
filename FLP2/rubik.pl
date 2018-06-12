% Rubikova kocka
% Peter Tisovcik (xtisov00)

% prevzate z input2.pl
% =============================================================
% nacita riadok z stdin, koniecpri LF, OEF
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).

% vyhladanie EOF, LF
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).

% citanie riadkov z stdin
read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
		read_lines(LLs), Ls = [L|LLs]
	).

% rozdeli riadok na podzoznamy
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]).

% vstupom je zoznam riadkov, kazdy riadok je zoznam znakov
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).
% =============================================================
% prevedenie vstupnej reprezentacie kocky na reprezentacie,
% s ktorou sa lepsie pracuje, jedna strana == deden zoznam
convertCube(
    [
    [[U1,U2,U3]],
    [[U4,U5,U6]],
    [[U7,U8,U9]],
    [[F1,F2,F3],[R1,R2,R3],[B1,B2,B3],[L1,L2,L3]],
    [[F4,F5,F6],[R4,R5,R6],[B4,B5,B6],[L4,L5,L6]],
    [[F7,F8,F9],[R7,R8,R9],[B7,B8,B9],[L7,L8,L9]],
    [[D1,D2,D3]],
    [[D4,D5,D6]],
    [[D7,D8,D9]]
    ],
    [
    [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    [F1,F2,F3,F4,F5,F6,F7,F8,F9],[R1,R2,R3,R4,R5,R6,R7,R8,R9],[B1,B2,B3,B4,B5,B6,B7,B8,B9],[L1,L2,L3,L4,L5,L6,L7,L8,L9],
    [D1,D2,D3,D4,D5,D6,D7,D8,D9]
    ]
).

% rotacia po smere hodinovych ruciciek
% horna
up([[U1, U2, U3, U4, U5, U6, U7, U8, U9],
	[F1, F2, F3, F4, F5, F6, F7, F8, F9],
	[R1, R2, R3, R4, R5, R6, R7, R8, R9],
	[B1, B2, B3, B4, B5, B6, B7, B8, B9],
	[L1, L2, L3, L4, L5, L6, L7, L8, L9],
	D],
	[[U7, U4, U1, U8, U5, U2, U9, U6, U3],
	[R1, R2, R3, F4, F5, F6, F7, F8, F9],
	[B1, B2, B3, R4, R5, R6, R7, R8, R9],
	[L1, L2, L3, B4, B5, B6, B7, B8, B9],
	[F1, F2, F3, L4, L5, L6, L7, L8, L9],
	D]).

% predna
front([[U1, U2, U3, U4, U5, U6, U7, U8, U9],
	 [F1, F2, F3, F4, F5, F6, F7, F8, F9],
	 [R1, R2, R3, R4, R5, R6, R7, R8, R9],
	 B,
	 [L1, L2, L3, L4, L5, L6, L7, L8, L9],
	 [D1, D2, D3, D4, D5, D6, D7, D8, D9]],
	 [[U1, U2, U3, U4, U5, U6, L9, L6, L3],
	 [F7, F4, F1, F8, F5, F2, F9, F6, F3],
	 [U7, R2, R3, U8, R5, R6, U9, R8, R9],
	 B,
	 [L1, L2, D1, L4, L5, D2, L7, L8, D3],
	 [R7, R4, R1, D4, D5, D6, D7, D8, D9]]).

% prava
right([[U1, U2, U3, U4, U5, U6, U7, U8, U9],
	[F1, F2, F3, F4, F5, F6, F7, F8, F9],
	[R1, R2, R3, R4, R5, R6, R7, R8, R9],
	[B1, B2, B3, B4, B5, B6, B7, B8, B9],
	L,
	[D1, D2, D3, D4, D5, D6, D7, D8, D9]],
	[[U1, U2, F3, U4, U5, F6, U7, U8, F9],
	[F1, F2, D3, F4, F5, D6, F7, F8, D9],
	[R7, R4, R1, R8, R5, R2, R9, R6, R3],
	[U9, B2, B3, U6, B5, B6, U3, B8, B9],
	L,
	[D1, D2, B7, D4, D5, B4, D7, D8, B1]]).

% zadna
back([[U1, U2, U3, U4, U5, U6, U7, U8, U9],
	F,
	[R1, R2, R3, R4, R5, R6, R7, R8, R9],
	[B1, B2, B3, B4, B5, B6, B7, B8, B9],
	[L1, L2, L3, L4, L5, L6, L7, L8, L9],
	[D1, D2, D3, D4, D5, D6, D7, D8, D9]],
	[[R3, R6, R9, U4, U5, U6, U7, U8, U9],
	F,
	[R1, R2, D9, R4, R5, D8, R7, R8, D7],
	[B7, B4, B1, B8, B5, B2, B9, B6, B3],
	[U3, L2, L3, U2, L5, L6, U1, L8, L9],
	[D1, D2, D3, D4, D5, D6, L1, L4, L7]]).

% lava
left([[U1, U2, U3, U4, U5, U6, U7, U8, U9],
	[F1, F2, F3, F4, F5, F6, F7, F8, F9],
	R,
	[B1, B2, B3, B4, B5, B6, B7, B8, B9],
	[L1, L2, L3, L4, L5, L6, L7, L8, L9],
	[D1, D2, D3, D4, D5, D6, D7, D8, D9]],
	[[B9, U2, U3, B6, U5, U6, B3, U8, U9],
	[U1, F2, F3, U4, F5, F6, U7, F8, F9],
	R,
	[B1, B2, D7, B4, B5, D4, B7, B8, D1],
	[L7, L4, L1, L8, L5, L2, L9, L6, L3],
	[F1, D2, D3, F4, D5, D6, F7, D8, D9]]).

% dolna
down([U,
	[F1, F2, F3, F4, F5, F6, F7, F8, F9],
	[R1, R2, R3, R4, R5, R6, R7, R8, R9],
	[B1, B2, B3, B4, B5, B6, B7, B8, B9],
	[L1, L2, L3, L4, L5, L6, L7, L8, L9],
	[D1, D2, D3, D4, D5, D6, D7, D8, D9]],
	[
	U,
	[F1, F2, F3, F4, F5, F6, L7, L8, L9],
	[R1, R2, R3, R4, R5, R6, F7, F8, F9],
	[B1, B2, B3, B4, B5, B6, R7, R8, R9],
	[L1, L2, L3, L4, L5, L6, B7, B8, B9],
	[D7, D4, D1, D8, D5, D2, D9, D6, D3]]).

% rotacia v smere hodinovych ruciciek a naopak
rotation(Start,End) :- up(Start,End).
rotation(Start,End) :- up(End,Start).
rotation(Start,End) :- front(Start,End).
rotation(Start,End) :- front(End,Start).
rotation(Start,End) :- down(Start,End).
rotation(Start,End) :- down(End,Start).
rotation(Start,End) :- right(Start,End).
rotation(Start,End) :- right(End,Start).
rotation(Start,End) :- back(Start,End).
rotation(Start,End) :- back(End,Start).
rotation(Start,End) :- left(Start,End).
rotation(Start,End) :- left(End,Start).

% iterativne DS
findRotations(Start, 0, [Start]) :- isSolved(Start).
findRotations(Start, Length, [Start|Moves]) :- Length > 0, rotation(Start, Res), L is Length - 1, findRotations(Res, L, Moves).

% hladanie postupnosti pohybu pre zlozenie kocky
solveCube(Start, Length, Moves) :- findRotations(Start, Length, Moves).
solveCube(Start, Length, Moves) :- L is Length + 1, solveCube(Start, L, Moves).

% test, ci je kocka zlozena
face_solved(Face) :- list_to_set(Face, [_]).
isSolved([U,F,R,B,L,D]) :- face_solved(U), face_solved(F), face_solved(R), face_solved(B), face_solved(L), face_solved(D).

% print riadok
printLine([]) :- nl.
printLine([H|T]) :- writef("%s ", [H]), printLine(T).

% print riadky
printLines([]).
printLines([H|T]):-
	printLine(H),
	printLines(T).

% prevedie kocku do vstupnej reprezentacie a zobrazi ju
writeMoves([]).
writeMoves([H]) :- convertCube(Res, H), printLines(Res).
writeMoves([H|T]) :- convertCube(Res, H), printLines(Res), nl, writeMoves(T).

main :-
    prompt(_, ''),
    read_lines(In),              % nacitanie vstupnej reprezentacie kocky
    split_lines(In,LoadedData),  % rozdelenie riadkov na podzoznamy
    convertCube(LoadedData,Cube),% vstupna reprezentacia na vnutornu
    solveCube(Cube, 0, Out),     % riesenie kocky
    writeMoves(Out),             % vypis kocky
    halt.
