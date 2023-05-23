:- use_module(library(clpfd)).

/* N QUEENS */
n_queens(N, Qs) :-
	length(Qs, N),
	Qs ins 1..N,
	safe_queens(Qs),
	labeling([ff], Qs).

safe_queens([]).
safe_queens([Q|Qs]) :-
	safe_queens(Qs, Q, 1),
	safe_queens(Qs).

safe_queens([], _, _).
safe_queens([Q|Qs], Q0, D0) :-
	Q0 #\= Q,
	abs(Q0 - Q) #\= D0,
	D1 #= D0 + 1,
	safe_queens(Qs, Q0, D1).
	

/* SUDOKU */
readMyFile :-
    open('rules.txt', read, Str),
    read_file(Str,Lines),
    close(Str),
	empty_matrix(9, Rows), !, fill_matrix(Lines, Rows, 9), sudoku(Rows), writeList(Rows).

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).

empty_matrix(N, Matrix) :-
    length(Matrix, N),
    maplist(same_length(Matrix), Matrix).

writeList([]) :- nl. 
writeList([X|T]) :- 
   	write(X), nl,
    writeList(T).
% Fill the matrix
fill_matrix([], _, _).
fill_matrix([f(X, Y, V)|Rest], Matrix, N) :-
    X1 is X-1, Y1 is Y-1,
    f(X1, Y1, V, Matrix),
    fill_matrix(Rest, Matrix, N).

f(X, Y, V, Matrix) :-
    nth0(X, Matrix, Row),   % Get the Yth row of the matrix
    nth0(Y, Row, V).        % Set the Xth element of the row to V

sudoku(Rows) :-
        length(Rows, 9), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..9,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        Rows = [A,B,C,D,E,F,G,H,I],
        blocks(A, B, C), blocks(D, E, F), blocks(G, H, I).

blocks([], [], []).
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
        all_distinct([A,B,C,D,E,F,G,H,I]),
        blocks(Bs1, Bs2, Bs3).

/* CryptoArithmetic Solver */
cryptArithmetic([H1|T1], [H2|T2], [H3|T3]):- 
    append([H1|T1],[H2|T2],Temp), 
    append(Temp,[H3|T3],FullList),

    list_to_set(FullList, CurrSet),
    CurrSet ins 0..9,
    all_distinct(CurrSet), 

    % make sure the heads are not zeros
    H1 #> 0, 
	H2 #> 0, 
	H3 #> 0,
    
	length(T1,T11),
	length(T2,T21),
	length(T3,T31),
    convertToNum([H1|T1],N1,T11),
    convertToNum([H2|T2],N2,T21),
    convertToNum([H3|T3],N3,T31),
    N3 #= N1 + N2,

    labeling([], CurrSet).

convertToNum([],0, _).
convertToNum([H|T],V, K) :- 
    length(T,L), Base #= 10^L, 
    BaseValue #= Base * H,
    
    V #= V1 + BaseValue,
    convertToNum(T,V1, K).

/* Zebra */

solve(Men, Color, Cigarette, Animal, Drink) :-
    
    Men ins 1..5, 
	Color ins 1..5,
	Cigarette ins 1..5, 
	Animal ins 1..5, 
	Drink ins 1..5,

    all_different(Men), all_different(Color), all_different(Cigarette), all_different(Animal), all_different(Drink),

    Men = [M1,M2,M3,M4,M5], Color = [C1,C2,C3,C4,C5], Cigarette = [CIG1,CIG2,CIG3,CIG4,CIG5], Animal = [A1,A2,A3,A4,A5], Drink = [D1,D2,D3,D4,D5],
    
    %Rule 1: Englishman lives in red house
	M1 #= C1,
	%Rule 2: Spaniard Owns Dog
	M2 #= A1,
	%Rule 3: Coffee in green house
	C2 #= D1,
	%Rule 4: ukrainian drinks tea
	M3 #= D2,
	%Rule 5:  Green house right of ivory
	C2 #= C3 + 1,
	%Rule 6: Winston owns snails
	CIG1 #= A2,
	%Rule 7: Kools in Yellow house
	CIG3 #= C4,
	%Rule 8: Milk in Middle house
	D3 #= 3,
	%Rule 9: Norwegian lives in first house to the left
	M5 #= 1,
	%Rule 10: Chesterfiled smoker lives next to man with fox
	A3 #= CIG2 + 1 #\/ A3 #= CIG2 - 1,
	%Rule 11: Kools next to horse
	A4 #= CIG3 + 1 #\/ A3 #= CIG3 - 1,
	%Rule 12: Lucky Strike drink oj
	CIG4 #= D4,
	%Rule 13: Japanese Parliament
	M4 #= CIG5,
	%Rule 14: Norwegian next to blue house
	Men5 #= C5 + 1 #\/ Men5 #= C5 - 1, 
	

    labeling([ff],Men), labeling([ff],Color), labeling([ff],Cigarette), labeling([ff],Animal), labeling([ff],Drink),

    H = [lives(englishman,M1), lives(spaniard,M2), lives(ukrainian,M3), lives(japanese,M4), lives(norwegian,M5)],
    
    member(lives(Owner1,A5), H), write('Who owns the zebra?'), nl, write(Owner1), nl,
    member(lives(Owner2,D5), H), write('Who Drinks Water?'), nl, write(Owner2), nl.






















