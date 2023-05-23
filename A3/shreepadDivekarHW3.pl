/*
 * Name:        <Shreepad> <Divekar> smd220000
 * Class:       CS6374
 * Assignment:  Homework 3
 * Details:     Contains Prolog source code and unit test cases to test the code.
 *              SWISH link: <Link to your file if there is a SWISH version>
 * Note:        <Additional Notes, e.g. filename for non-code questions.>
 * 
 * */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% Example For Custom Operators %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
Question: How do you redefine an built-in operator?
Answer:   You can't! Using the docs you can find how to define 
          your own oeprators though! 
          https://www.swi-prolog.org/pldoc/doc_for?object=op/3

Example:  Defining the `univ` operator `=..`, using the operator `..=`

    Prolog Built-In:
        > Term =.. [one, two]. 
          Term = one(two).

    Our Operator:
       > Term ..= [one, two]. 
         Term = one(two).
*/

% redefine the operator, 700 is the precedence number of =.. so ours
%   should have the same
% xfx denotes the type of the operator
% `x` means the precedence of the argument must strictly be lower than
%   the functor
% `f` denotes the functor, in this case the operator we're defining.

:- op(700, xfx, ..=).

..=(Term, [FunctorNonZeroArity|Args]) :-
    length(Args, N),
    \+(N =:= 0),
    functor(Term, FunctorNonZeroArity, N),
    insert_args(Term, Args), !. % we cut since there should only be one solution.


insert_args(Term, Args) :-
    insert_args(Term, Args, 1).

insert_args(_, [], _).
insert_args(Term, [NextArg|Args], N) :-
    arg(N, Term, NextArg),
    N1 is N + 1,
    insert_args(Term, Args, N1).
    
%% If this is true, then when the file is loaded there should be no error.
%% Try to make it incorrect and the error will be displayed as a warning.
:- Term ..= [invented, kowalski, prolog], Term =.. TermToList, ==(TermToList, [invented, kowalski, prolog]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% Ex. 11.3.1 %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% 11.3 (i)
%%   \== using == and cut-fail
%%   You will have to define the \== op as /==
%%   Consult the example above on how to do it.
%%   Your operator /== will be tested using infix notation!
%/==(_, _) :-
%    write("Error: Not Implemented"), false.
:- op(700, xfx, /==).
X /== Y :- X == Y, !, fail.
_ /== _.


%% 11.3 (ii)
%%   nonvar using var and cut-fail
%%   You will have to define the nonvar op as non+var
%non_var(_) :- 
%    write("Error: Not Implemented"), false.
:- op(700, xfx, 'non_var').

non_var(X) :- var(X), !, fail.
non_var(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Belgian Snake Problem %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%snake(RingAtoms, RowRingFormat, ColumnRingFormat) :-
%    write("Error: Not Implemented"), false.
%  snake([a,b,c,d],[_,_,_,_,_],[_,_,_]) abcda\nbadcb\ncdabc\n
snake(_, _, []).

snake(RingAtoms, RowRingFormat, [ColumnRingFormat]) :-
    makePattern(RingAtoms, RowRingFormat, ColumnRingFormat,_), writeList(ColumnRingFormat).

snake(RingAtoms,RowRingFormat,[C1,C2|CT]) :- 
    makePattern(RingAtoms,RowRingFormat,C1,RingAtoms1),
    makePattern(RingAtoms1,RowRingFormat,Temp,P2),
    reverse(Temp,C2),
    writeList(C1), writeList(C2),
    snake(P2,RowRingFormat,CT).

% rotate([a,b,c],[b,c,a]) is true.
headToENd([],[]).
headToENd([X|T],R) :-
    append(T,[X],R).

makePattern(Plast, [], [], Plast).
makePattern([P|Ps], [_|Rs], [P|Ls], Plast) :-
    headToENd([P|Ps], Pnext),
    makePattern(Pnext, Rs, Ls, Plast).

writeList([]) :- nl.
writeList([H|T]) :- write(H), writeList(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% N-Queens Problem %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
 * data representation
 * 	Qs = [R1, R2, R3, R4].
 * 	Here R1 through R4 are unique row numbers, and their position
 * 	in the list is the column number, this ensures no two queens
 *  are in the same row or column.
 *
 * Thus, a queen can attack another iff they lie on the same
 * diagonal.
 * */


 %queens(N, Qs) :-
 %   write("Error: Not Implemented"), false.


queens(N,Qs) :- 
    make_domain(1,N,Ns), permutate(Ns,Qs), safe(Qs).

% Safe/Attack taken from notes
% The placement Qs is safe/ valid.
safe([]).
safe([Q|Qs]) :- 
    safe(Qs), \+ attack(Q,Qs).

attack(X,Xs) :- attack(X,1,Xs).
attack(X,N,[Y|_]) :- 
    X is Y+N; X is Y-N.
attack(X,N,[_|Ys]) :- 
    N1 is N+1, attack(X,N1,Ys).

% make_domain(M,N,List): List is list of integers [M,N], M<N. 
%Basically ?- make_domain(1,5,N).  N= [1,2,3,4,5]
make_domain(N,N,[N]).
make_domain(M,N,[M|Ns]) :- 
    M < N, M1 is M+1, make_domain(M1,N,Ns).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% Cryptarithmetic Puzzles %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%solve(Vars1, Vars2, VarsTotal) :-
%    write("Error: Not Implemented"), false.

solve(Vars1, Vars2, VarsTotal) :-
    reverse(Vars1, L1r), reverse(Vars2, L2r), reverse(VarsTotal, L3r),
    solver(L1r, L2r, L3r, 0, [0,1,2,3,4,5,6,7,8,9]), % 0 is for carry
    headCheck(Vars1), headCheck(Vars2), headCheck(VarsTotal).

solver([],[],[],0,_).
solver([],[], [C], C, _).
solver([H1|T1], [H2|T2],  [H3|T3], C, L):-
    (var(H1) -> select(H1, L, L1); L1=L),
    (var(H2) -> select(H2, L1, L2); L2=L1),
    (var(H3) -> H3 is (H1+H2+C) mod 10, select(H3, L2, L3); H3 is (H1+H2+C) mod 10, L3=L2),
    C1 is (H1+H2+C) // 10,
    solver(T1, T2, T3, C1, L3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% Stable Marriage Problem %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Men and Women are a list of atoms
%% Men = [m1, m2], Women = [w1, w2].

%% Men'sPrefsLists and Women'sPrefsLists are a list of list of atoms
%% MensPrefsLists = [[w1, w2], [w2, w1]], WomensPrefsLists = [[m1, m2], [m2, m1]].

%% Marriages are a list of lists of [man, woman] married pair.
%% Marriages = [[m1, w1], [m2, w2]]

stable_marriages(Men, MensPrefsLists, Women, WomensPrefLists, Marriages) :-
    write("Error: Not Implemented"), false.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Blocks World Problem %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
block(a).
block(b).
block(c).
block(d).
block(e).

place(p).
place(q).
place(r).

%transform(State1, State2, Plan) :-
%    write("Error: Not Implemented"), false.
transform(Statel,State2,Plan) :-
	transform(Statel,State2, [Statel], Plan).

transform(State,State,_,[]).
transform(Statel,State2,Visited, [Action|Actions]) :-
    choose_action(Action,Statel, State2),
	update(Action,Statel ,State),
	\+ member(State,Visited),
	transform(State,State2, [State|Visited] ,Actions).

legal_action(to_place(Block,Y,Place) ,State) :-
	on(Block,Y,State), clear(Block,State),
	place(Place), clear(Place,State).

legal_action(to_block(Block1,Y,Block2), State) :-
	on(Blockl,Y,State), clear(Blockl,State), block(Block2),
	Block1 \= Block2, clear(Block2,State).

clear(X,State) :- \+ member(on(_,X),State).

on(X,Y,State) :- member(on(X,Y) ,State).

update(to_block(X,Y,Z) ,State,State1) :-
	substitute(on(X,Y),on(X,Z),State,State1).

update(to_place(X,Y,Z),State,Statel) :-
	substitute(on(X,Y),on(X,Z),State,Statel).

substitute(_,_,[],[]).
substitute(X,Y,[X|Xs],[Y|Ys]):-substitute(X,Y,Xs,Ys).
substitute(X,Y,[Z|Xs],[Z|Ys]):-X\=Z, substitute(X,Y,Xs,Ys).

choose_action(Action,Statel,State2) :-
	suggest(Action,State2), legal_action(Action,Statel).

choose_action(Action,State1,_) :-
	legal_action(Action,State1).

suggest(to_place(X,_,Z) ,State) :-
	member(on(X,Z),State), place(Z).
suggest(to_block(X,_,Z) ,State) :-
	member(on(X,Z),State), block(Z).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Missionary-Cannibal Problem %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Shore States
%   left_shore(NumMissionary, NumCannibal).
%   right_shore(NumMissionary, NumCannibal).

% Moves Should Be Given By
%   boat(NumMissionary, NumCannibal).

% Your input will be the initial shore states, and the output will be the sequence
% of moves; any valid sequence of moves should be admissible. And the final states.

/*
Challenge: Print out the states before and after each move
Note: It is not necessary to implement this!
    
    Initial State           Move        Final State
    left_shore(3, 3)      boat(0,2)     left_shore(3, 1)
    right_shore(0, 0)                   right_shore(0, 2)
*/

%get_moves(LeftShoreInit, RightShoreInit, Moves, LeftShoreFinal, RightShoreFinal) :-
%    write("Error: Not Implemented"), false.
initPos(left).
initPos(right).
boat(_,_).

abs_val(X, X) :- X >= 0.
abs_val(X, Y) :- X < 0, Y is -X.

legalMove(CL, ML, CR, MR) :-
	% Check if this  state is allowed
	ML>=0, CL>=0, MR>=0, CR>=0,
	(ML>=CL ; ML=0),
	(MR>=CR ; MR=0).

move([left_shore(ML,CL), initPos(left),right_shore(MR,CR)],[left_shore(ML2,CL), initPos(right),right_shore(MR2,CR)]):-
     % Two missionaries cross left to right.
     MR2 is MR+2,
	 ML2 is ML-2,
	 legalMove(CL, ML2, CR, MR2).
     

move([left_shore(ML,CL), initPos(left),right_shore(MR,CR)],[left_shore(ML,CL2), initPos(right),right_shore(MR,CR2)]):-
	% Two cannibals cross left to right.
	CR2 is CR+2,
	CL2 is CL-2,
	legalMove(CL2,ML,CR2,MR).

move([left_shore(ML,CL), initPos(left),right_shore(MR,CR)],[left_shore(ML2,CL2), initPos(right),right_shore(MR2,CR2)]):-
	%  One missionary and one cannibal cross left to right.
	CR2 is CR+1,
	CL2 is CL-1,
	MR2 is MR+1,
	ML2 is ML-1,
	legalMove(CL2,ML2,CR2,MR2).

move([left_shore(ML,CL), initPos(left),right_shore(MR,CR)],[left_shore(ML2,CL), initPos(right),right_shore(MR2,CR)]):-
	% One missionary crosses left to right.
	MR2 is MR+1,
	ML2 is ML-1,
	legalMove(CL,ML2,CR,MR2).

move([left_shore(ML,CL), initPos(left),right_shore(MR,CR)],[left_shore(ML,CL2), initPos(right),right_shore(MR,CR2)]):-
	% One cannibal crosses left to right.
	CR2 is CR+1,
	CL2 is CL-1,
	legalMove(CL2,ML,CR2,MR).


move([left_shore(ML,CL), initPos(right),right_shore(MR,CR)],[left_shore(ML2,CL), initPos(left),right_shore(MR2,CR)]):-
     % Two missionaries cross right to left.
     MR2 is MR-2,
	 ML2 is ML+2,
	 legalMove(CL,ML2,CR,MR2).
     

move([left_shore(ML,CL), initPos(right),right_shore(MR,CR)], [left_shore(ML,CL2), initPos(left),right_shore(MR,CR2)]):-
	% Two cannibals cross right to left.
	CR2 is CR-2,
	CL2 is CL+2,
	legalMove(CL2,ML,CR2,MR).

move([left_shore(ML,CL), initPos(right),right_shore(MR,CR)],[left_shore(ML2,CL2), initPos(left),right_shore(MR2,CR2)]):-
	%  One missionary and one cannibal cross right to left.
	CR2 is CR-1,
	CL2 is CL+1,
	MR2 is MR-1,
	ML2 is ML+1,
	legalMove(CL2,ML2,CR2,MR2).

move([left_shore(ML,CL), initPos(right),right_shore(MR,CR)],[left_shore(ML2,CL), initPos(left),right_shore(MR2,CR)]):-
	% One missionary crosses right to left.
	MR2 is MR-1,
	ML2 is ML+1,
	legalMove(CL,ML2,CR,MR2).

move([left_shore(ML,CL), initPos(right),right_shore(MR,CR)], [left_shore(ML,CL2), initPos(left),right_shore(MR,CR2)]):-
	% One cannibal crosses right to left.
	CR2 is CR-1,
	CL2 is CL+1,
	legalMove(CL2,ML,CR2,MR).

get_moves(left_shore(ML1,CL1), right_shore(MR1,CR1), Moves,
          left_shore(ML_Final,CL_Final), right_shore(MR_Final,CR_Final)) :-
          missionaryCannibal([left_shore(ML1,CL1), initPos(left), right_shore(MR1,CR1)],
                         [left_shore(ML_Final,CL_Final), initPos(right), right_shore(MR_Final,CR_Final)],
                         [left_shore(3,3), initPos(right), right_shore(0,0)],[],Moves).
    
missionaryCannibal([left_shore(ML1,CL1), initPos(B1), right_shore(MR1,CR1)],
                         [left_shore(ML_Final,CL_Final), initPos(B_Final), right_shore(MR_Final,CR_Final)],
                         ExploredStates,MovesList,Moves):-
                       
        [left_shore(ML1,CL1), initPos(B1), right_shore(MR1,CR1)] \= 
        [left_shore(ML_Final,CL_Final), initPos(right), right_shore(MR_Final,CR_Final)],
          move([left_shore(ML1,CL1), initPos(B1), right_shore(MR1,CR1)],
                 [left_shore(ML2,CL2), initPos(B2), right_shore(MR2,CR2)]), % make an allowed move
   \+member([left_shore(ML2,CL2), initPos(B2), right_shore(MR2,CR2)],ExploredStates),
      C1 is CL2 - CL1,
           M1 is ML2 - ML1,
             abs_val(C1, C2),
             abs_val(M1, M2),
         missionaryCannibal([left_shore(ML2,CL2), initPos(B2), right_shore(MR2,CR2)],
                         [left_shore(ML_Final,CL_Final), initPos(B_Final), right_shore(MR_Final,CR_Final)],
    [[left_shore(ML1,CL1), initPos(B1), right_shore(MR1,CR1)] | ExploredStates],
             [[[left_shore(ML1,CL1), initPos(B1), right_shore(MR1,CR1)]] | MovesList ],
             [boat(M2,C2) | Moves]).


missionaryCannibal([left_shore(ML_Final,CL_Final), initPos(right), right_shore(MR_Final,CR_Final)],
                   [left_shore(ML_Final,CL_Final), initPos(right), right_shore(MR_Final,CR_Final)]
                   ,_,_, Moves):- reverse(Moves,Movesr),
writeMoves(Movesr).
        
writeMoves([]) :- nl. 
writeMoves([X|T]) :- 
   	write(X), nl,
    writeMoves(T).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% Unit Tests  %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Helper Code
% Implemented in Class
reverse(List, Reversed) :-
    reverse(List, [], Reversed).

reverse([], Reversed, Reversed).
reverse([X|Xs], Acc, Reversed) :-
    reverse(Xs, [X|Acc], Reversed).

headCheck([H|_]) :- H \= 0.
headCheck([]) :- false.

% Implementation from Class
% permutate(L,P): P is a permutation of L
permutate([],[]).
permutate(Xs,[Z|Zs]) :-
    select(Z,Xs,Ys), permutate(Ys,Zs).

% select(X,List,R): R is a List with one removed X
select(X,[X|T],T).
select(X,[H|T],[H|R]) :- %X \= H, 
    select(X,T,R).

% Helper Code: Don't Remover or Modify %
dict_items(Dict, Keys, Values) :-
    dict_pairs(Dict, _, KVPairs),
    pairs_keys(KVPairs, Keys),
    pairs_values(KVPairs, Values).

% Helper Code: Don't Remover or Modify %
stable_marriages_test(MensPrefs, WomensPrefs, Marriages) :-
    dict_items(MensPrefs, Men, MensPrefsLists),
    dict_items(WomensPrefs, Women, WomensPrefLists),
    stable_marriages(Men, MensPrefsLists, Women, WomensPrefLists, Marriages).


:- begin_tests(home_work_3).

test(solve, nondet) :-
    solve([S,E,N,D], [M,O,R,E], [M,O,N,E,Y]),
    D is 7, E is 5, M is 1, N is 6, O is 0, R is 8,
    S is 9, Y is 2.

test(/==) :-
    /==(1, 2), \+(/==(1, 1)),
    current_op(_, _, /==),
    term_string(Term1, "1 /== 2"),
    call(Term1),
    term_string(Term2, "\\+(1 /== 1)"),
    call(Term2).
    

test(non_var) :-
    non_var(a), not(non_var(_)).

test(snake, Output='abcda\nbadcb\ncdabc\n') :-
    with_output_to(atom(Output), snake([a,b,c,d],[_,_,_,_,_],[_,_,_])).

test(queens, nondet) :-
    queens(4, [3, 1, 4, 2]),
    queens(10, [7, 4, 2, 9, 5, 10, 8, 6, 3, 1]).

test(stable_marriages, nondet) :-
    
    stable_marriages_test(
        mens_preference{
            david: [judy, paula],
            jeremy: [judy, paula]
        },
        womens_preference{
            paula: [david, jeremy],
            judy: [david, jeremy]
        },
        [[david, judy], [jeremy, paula]]
    ),

    stable_marriages_test(
        mens_preferences{
            avraham: [chana, tamar, zvia, ruth, sarah],
            binyamin: [zvia, chana, ruth, sarah, tamar],
            chaim: [chana, ruth, tamar, sarah, zvia],
            david: [zvia, ruth, chana, sarah, tamar],
            elazar: [tamar, ruth, chana, zvia, sarah]
        },
        womens_preferences{
            zvia: [elazar, avraham, david, binyamin, chaim],
            chana: [david, elazar, binyamin, avraham, chaim],
            ruth: [avraham, david, binyamin, chaim, elazar],
            sarah: [chaim, binyamin, david, avraham, elazar],
            tamar: [david, binyamin, chaim, elazar, avraham]
        },
        [[avraham, ruth], [binyamin, sarah], [chaim, tamar], [david, chana], [elazar, zvia]]
    ).

test(get_moves, nondet) :-
  get_moves(
    left_shore(3, 3), 
    right_shore(0, 0),
    [
        boat(0, 2), 
        boat(0, 1), 
        boat(0, 2), 
        boat(0, 1), 
        boat(2, 0), 
        boat(1, 1), 
        boat(2, 0), 
        boat(0, 1), 
        boat(0, 2), 
        boat(0, 1), 
        boat(0, 2)
    ],
    left_shore(0, 0),
    right_shore(3, 3)
).

test(transform, nondet) :-
  
  transform(
    [on(a, b), on(b, p), on(c, r)], 
    [on(a, b), on(b, c), on(c, r)],
    [
        to_place(a, b, q), 
        to_block(b, p, c), 
        to_block(a, q, b)
    ]
  ).

:- end_tests(home_work_3).
