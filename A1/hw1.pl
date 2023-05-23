/*
 * Name: 		Shreepad Divekar (smd220000)
 * Class: 		CS6374
 * Assignment: Homework 1
 * Details:		Contains Prolog source code and unit test cases to test the code.
 * 				SWISH link: <Link to your file if there is a SWISH version>
 * Note:		   <Additional Notes, e.g. filename for non-code questions.>
 * 
 * */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q1      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For gender use - male(NAME) or female(NAME).
% For parents use - father(FATHER_NAME, CHILD_NAME) or mother(MOTHER_NAME, CHILD_NAME)
% For married partners use - married(WIFE_NAME, HUSBAND_NAME)

% START Family Facts Enumeration

female(lisa).
female(nancy).
female(mary).
female(jill).
female(sarah).
female(susan).
female(martha).
female(kim).
female(ann).

male(abe).
male(john).
male(rick).
male(tony).
male(bill).
male(rob).
male(jack).
male(phil).
male(jim).

mother(lisa, abe).
mother(lisa, sarah).
mother(nancy, john).
mother(mary, jill).
mother(sarah, susan).
mother(susan, jack).
mother(susan, phil).

father(tony, abe).
father(tony, sarah).
father(abe, john).
father(john, jill).
father(bill, susan).
father(rob, phil).
father(rob, jack).
father(jack, jim).

married(lisa, tony).
married(nancy, abe).
married(sarah, bill).
married(mary, john).
married(susan, rob).
married(jill, rick).
married(kim, jack).
married(martha, jim).
married(ann, phil).
% END of Family Facts

parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).

%parent(X, Y) :-
 %  write("Error: Not Implemented"), false.

grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
greatgrandparent(X, Y) :- parent(X, Z), grandparent(Z, Y).
%grandparent(X, Y) :-
 %  write("Error: Not Implemented"), false.
sibling(X,Y) :- parent(Z,X), parent(Z,Y), not(X=Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q2      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q2(a) 
fcousin(X,Y):- parent(Z,X), parent(W,Y), sibling(Z,W).
%fcousin(X, Y) :-
%   write("Error: Not Implemented"), false.

% Q2(b)
scousin(X,Y):- grandparent(Z,X), grandparent(W,Y), sibling(Z,W).
%scousin(X, Y) :-
%   write("Error: Not Implemented"), false.

% Q2(c)
nephew(X,Y) :- sibling(Z,Y), parent(Z,X), male(X).
nephew(X,Y) :- fcousin(Z,Y), parent(Z,X), male(X).
grnephew(X,Y) :- nephew(Z,Y), parent(Z,X), male(X).
grnephew(X,Y) :- niece(Z,Y), parent(Z,X), male(X).
%grnephew(X, Y) :- 
%   write("Error: Not Implemented"), false.

% Q2(d)
niece(X,Y) :- sibling(Z,Y), parent(Z,X), female(X).
niece(X,Y) :- fcousin(Z,Y), parent(Z,X), female(X).
%niece(X, Y) :- 
%   write("Error: Not Implemented"), false.

% Q2(e)
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).
manc(X,Y) :- male(X), ancestor(X,Y).
%manc(X, Y) :-
%   write("Error: Not Implemented"), false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q3      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q3:
generation_cousin(X, Y) :- parent(Z, X), parent(Z, Y).
generation_cousin(X, Y) :- parent(P1, X), parent(P2, Y), generation_cousin(P1,P2).
%generation_cousin(X, Y) :-
%   write("Error: Not Implemented"), false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q4      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q4(a)
grnephew_mar(X,Y):- grnephew(X,Y).
grnephew_mar(X,Y):- married(Y,Z), grnephew(X,Z).
%grnephew_mar(X, Y) :- 
%   write("Error: Not Implemented"), false.

% Q4(b)
niece_mar(X,Y) :- niece(X,Y).
niece_mar(X,Y) :- married(Y,Z), niece(X,Z).
niece_mar(X,Y) :- married(Z,Y), niece(X,Z).
%niece_mar(X, Y) :- 
%   write("Error: Not Implemented"), false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q5      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% <Q5.pdf>


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q6      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q6: Peano Arithmetic

% Peano Numbers
num(0).
num(s(X)) :- num(X).

% Helper func, converts decimal repr of peano num and back.
peano_to_decimal_rec(0, 0).
peano_to_decimal_rec(s(X), Y) :- peano_to_decimal_rec(X, N), Y is N + 1.

peano_to_decimal(X, Y) :- once(peano_to_decimal_rec(X, Y)).

% Comparison

% equal
equal(X, X).

% less than (add base cases when needed)
less_than(0,s(_)).
less_than(s(X),s(Y)) :- less_than(X,Y).
%less_than(X, Y) :-
%   write("Error: Not Implemented"), false.

% greater than (add base cases when needed)
greater_than(s(_),0).
greater_than(s(X),s(Y)) :- greater_than(X,Y).
%greater_than(X, Y) :-
%   write("Error: Not Implemented"), false.

% addition (add base cases when needed)
add(0,N,N).
add(s(N),M,s(R)) :- add(N,M,R).
%add(Addend, Augend, Sum) :- 
%   write("Error: Not Implemented"), false.

% multiplication (add base cases when needed)
multiply(0,_,0).
multiply(s(N),M,R) :- multiply(N,M,P), add(P,M,R).
%multiply(Multiplicand, Multiplier, Product) :-
%   write("Error: Not Implemented"), false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q7      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q7: Peano Factorial
% Try to make it reversible. 
% HINT: See the implementation of `peano_to_decimal`.
% There are multiple ways of making the function reversible, the hint is one way.
factorial(0,s(0)).
factorial(s(N), R) :- factorial(N, P), multiply(s(N), P, R).
%factorial(Number, Value) :-
%   write("Error: Not Implemented"), false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q8      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q8: Quotient And Remainder

/* Subtract Helper Function */
sub(N,0,N).
sub(s(N),s(M),R) :- sub(N,M,R).

/* Greater than Equal Helper Function */
greater_than_equal(0,0).
greater_than_equal(s(_),0).
greater_than_equal(s(X),s(Y)) :- greater_than_equal(X,Y).

divide(X, Y, Q, R) :- divide(X, Y, Q, 0, R).
divide(0, _, 0, 0, 0).
divide(X, 1, X, 0, 0).
divide(X, Y, Q, Q, X) :- less_than(X,Y).
divide(X, Y, Q, TmpQ, R) :- greater_than_equal(X,Y), sub(X, Y, NewX),
    divide(NewX, Y, Q, s(TmpQ), R).

%divide(Dividend, Divisor, Quo, Rem) :- 
%   write("Error: Not Implemented"), false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q9      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q9: Nth Fibonacci Number
fib(N, F) :-
  fib(N, F, F).
fib(0, 0, _).
fib(s(0), s(0), _).
fib(s(s(N)), F, s(X)) :-
  fib(N, F1, X),
  fib(s(N), F2, X),
  add(F1, F2, F).

%fib(N, F) :- 
%   write("Error: Not Implemented"), false.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% Unit Tests  %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(home_work_1).

    /* Test 1: Q2(a) (Passed) */
    test(fcousin) :-
        setof(X-Y, (fcousin(X, Y), X @< Y), [john-susan]).

    /* Test 2: Q2(b) (Passed) */
    test(scousin) :-
        setof(X-Y, (scousin(X, Y), X @< Y), [jack-jill, jill-phil]).

    /* Test 3: Q2(c) (Passed) */
    test(grnephew) :-
        setof(X-Y, (grnephew(X, Y)), Ls),
        msort(Ls, Sorted),
        msort([jack-abe, phil-abe, jim-john], Sorted).

    /* Test 4: Q2(d) (Passed) */
    test(niece) :-
        setof(X-Y, (niece(X, Y)), Ls), 
        msort(Ls, Sorted),
        msort([jill-susan, susan-abe], Sorted).

    /* Test 5: Q2(e) (Passed) */
    test(manc) :-
        setof(X-jim, (manc(X, jim)), Ancestors),
        msort(Ancestors, Sorted),
        msort([bill-jim, jack-jim, rob-jim, tony-jim], Sorted).
        
    /* Test 6: Q3 */
	test(generation_cousin) :-
	    setof(X-Y, (generation_cousin(X, Y), X @< Y), [abe-sarah, jack-jill, jack-phil, jill-phil, john-susan]).

    /* Test 7: Q4(a) (Passed) */
    test(grnephew_mar) :-
        setof(X-Y, (grnephew_mar(X, Y)), Relatives),
        msort(Relatives, Sorted),
        msort(
            [jack-abe, jack-nancy, jim-john, jim-mary, phil-abe, phil-nancy],
            Sorted
        ).

    /* Test 8: Q4(b) (Passed) */
    test(niece_mar) :-
        setof(X-Y, (niece_mar(X, Y)), Relatives),
        msort(Relatives, Sorted),
        msort(
            [susan-nancy, jill-rob, jill-susan, susan-abe],
            Sorted
        ).


    /* Test 9: Q6 (Passed) */
    test(equal) :-
      peano_to_decimal(X, 1), equal(s(0), X).

    /* Test 10: Q6 (Passed) */
    test(less_than) :-
      peano_to_decimal(Ten, 10), 
      peano_to_decimal(Eleven, 11),
      less_than(Ten, Eleven).
      

    /* Test 11: Q6 (Passed) */
    test(greater_than) :-
        peano_to_decimal(Ten, 10), 
        peano_to_decimal(Eleven, 11),
        greater_than(Eleven, Ten).

    /* Test 12: Q6 (Passed) */
    test(add) :-
        peano_to_decimal(Ten, 10),
        peano_to_decimal(Eleven, 11),
        peano_to_decimal(TwentyOne, 21),
        add(Ten, Eleven, TwentyOne).

    /* Test 13: Q6 (Passed) */
    test(multiply) :-
        peano_to_decimal(Ten, 10),
        peano_to_decimal(Eleven, 11),
        peano_to_decimal(OneHundredTen, 110),
        multiply(Ten, Eleven, OneHundredTen).

    /* Test 14: Q7 (Passed) */
    test(factorial) :-
        peano_to_decimal(SevenTwenty, 720),
        peano_to_decimal(Six, 6),
        factorial(Six, SevenTwenty).

    /* Test 15: Q8 */
    test(divide) :-
        peano_to_decimal(Five, 5),
        peano_to_decimal(SeventyNine, 79),
        peano_to_decimal(Fifteen, 15),
        peano_to_decimal(Four, 4),
        divide(SeventyNine, Five, Fifteen, Four).

    /* Test 16: Q9 (Passed) */
    test(fib) :-
        peano_to_decimal(Twelve, 12),
        peano_to_decimal(OneFourFour, 144),
        fib(Twelve, OneFourFour).


:- end_tests(home_work_1).