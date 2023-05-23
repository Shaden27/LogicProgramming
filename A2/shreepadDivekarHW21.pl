/*
 * Name:        <Shreepad> <Divekar>
 * Class:       CS6374
 * Assignment:  Homework 2
 * Details:     Contains Prolog source code and unit test cases to test the code.
 *              SWISH link: <Link to your file if there is a SWISH version>
 * Note:        <Additional Notes, e.g. filename for non-code questions.>
 * 
 * */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% PART 1: Ex. 3.2.1 %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% (i) Sublist v/s Subsequence
subsequence([X|Xs], [X|Ys]) :-
    subsequence(Xs, Ys).

subsequence(Xs, [_|Ys]) :-
    subsequence(Xs, Ys).

subsequence([], _).

/*
The first set of rules defines the predicate append/3, which concatenates two lists together. The first rule says that when the first list is empty,
the result of appending it to the second list is the second list itself. The second rule says that when the first list is non-empty with a head X and a tail Xs,
the result of appending it to the second list is a new list with X as the head and the result of appending Xs and the second list as the tail.

The second set of rules defines the predicate subsequence/2, which checks if the first list is a subsequence of the second list.
The first rule says that if the first list starts with an element X that is also the head of the second list, then the rest of the first list is a subsequence of the rest of the second list.
The second rule says that if the first list is not empty but does not start with an element that is the head of the second list, then it may still be a subsequence of the rest of the second list.
The third rule says that the empty list is a subsequence of any list.
In summary, append/3 concatenates two lists together, while subsequence/2 checks if one list is a subsequence of another. 
*/




% (ii) Write recursive programs for adjacent and last that
% are semantically equiv. to the ones defined on pg. 62,
% WITHOUT using the append predicate.
adjacent(X, Y, Zs) :-
    write("Error: Not Implemented"), false.

last(X, Xs) :-
    write("Error: Not Implemented"), false.



% (iii) double(List, ListList) s.t.
% Every element in List appears twice in ListList
%     i.e. all permutations are admissible
%double(List, ListList) :-
%    write("Error: Not Implemented"), false.
double([], []).
double([X|Xs],[X,X|Ys]):- double(Xs,Ys).


% (iv) The size of proof tree as a function of the size 
% of the input list for prog. 3.16a and prog 3.16b.
% The exact size or an asymptotic bound, either is acceptable.

/*
 * <YOUR ANSWER GOES HERE :: DON"T USE A SEPARATE FILE>
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% PART 2: Ex. 3.3.1 %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% (i) substitute(X, Y, L1, L2) <-
%	  	L2 is a result of substituting all occurences of X in L1 with Y
% Suggestion: name your implementation substitute_helper, and call
%   it within substitute.

substitute(X, Y, L1, L2) :-
    write("Error: Not Implemented"), false.



% (ii) What is the meaning of the given variant of select?
sel(X, [X|Xs], Xs).
sel(X, [Y|Ys], [Y|Zs]) :- X \= Y, sel(X, Ys, Zs).

/*
 * <YOUR ANSWER GOES HERE :: DON"T USE A SEPARATE FILE>
*/



% (iii) no_doubles(L1, L2) <- 
% 	where L2 is a result of removing all duplicates from L1
no_doubles(L1, L2) :-
    write("Error: Not Implemented"), false.



% (v) mergesort(Xs, Ys) <-
%	Ys is an ordered permutation of the list Xs
mergesort(Xs, Ys) :-
    write("Error: Not Implemented"), false.



% (vi) kth_largest(Xs, K, V) <-
%	Finds the kth largest element of the list Xs.
kth_largest(Xs, K, V) :-
    write("Error: Not Implemented"), false.



% (vii) better_poker_hand(Hand1, Hand2, Hand)
%	Where Hand is the better of the two hands.
% Conventions:
%     card(Suit, Value)
%     Suit ∈ {heart, diamond, spade, club}
%     Value ∈ {2,3,4,5,6,7,8,9,10,jack,queen,king,ace}
%     House Rule: no baby straight/5-high straight i.e. A 2 3 4 5
better_poker_hand(Hand1, Hand2, Hand) :-
    write("Error: Not Implemented"), false.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Start of Helper Functions %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper func, converts decimal repr of peano num and back.
dec_peano(0, 0).
dec_peano(X, s(Y)) :- dec_peano(N, Y), X is N + 1.

peano_dec(P, D) :- once(dec_peano(D, P)).

rangedec(0, [0]).
rangedec(s(N), [s(N)|L]) :-
    rangedec(N, L).

rangedec(N, N, [H]) :-
    rangedec(N, [H|_]).

rangedec(s(N), Stop, [s(N)|L]) :-
    rangedec(N, Stop, L).

% Numbers
num(0).
num(s(X)) :- num(X).

% Comparison

% less than
lt(0, s(X)) :- num(X).
lt(s(X), s(Y)) :- lt(X, Y).

% greater than
gt(s(X), s(Y)) :- lt(s(Y), s(X)).

% equal
eq(X, X).

% less than or equal
lteq(N1, N2) :-
    lt(N1, N2);
    eq(N1, N2).

% plus
add(0, Y, Y) :- num(Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

% times
times(0, Y, 0) :- num(Y).
times(s(X), Y, Z) :- times(X, Y, Z1), add(Y, Z1, Z).

%%%%%%%%%%%%%%%% Don't Remove Any Helper Functions %%%%%%%%%%%%%%%%%

% add a number to sbt
insert(N, node, node(N, node, node)).
insert(N, node(V, A, B), node(V, AA, B)) :- lteq(N, V), insert(N, A, AA).
insert(N, node(V, A, B), node(V, A, BB)) :- gt(N, V), insert(N, B, BB).


%  applies a predicate which will modify the tree
%  at a node with value V in list of values
tree_map_mutate(_, [], Old, Old).
tree_map_mutate(F, [X|Xs], Old, New) :-
    call(F, X, Old, Mid),
    tree_map_mutate(F, Xs, Mid, New).


% insert a list of numbers
inslist(Xs, Old, New) :-
    tree_map_mutate(insert, Xs, Old, New).


% delete a list of numbers 
% Note: this function will work
%    only if your implementation of
%    delete is correct.
dellist(Xs, Old, New) :-
    tree_map_mutate(delete, Xs, Old, New).


% get max value from tree
get_max(node(V, _, node), V).
get_max(node(_, _, R), M) :- get_max(R, M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% End of Helper Functions %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% PART 3: SBT Functions %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (i) sumtree(T, N): N is the sum of elements in SBT T (use succ arithmetic).
sumtree(T, N) :-
    write("Error: Not Implemented"), false.


% (ii) delete(E, T, Tn): delete the element E from SBT T to obtain SBT Tn.
delete(E, T, Tn) :-
    write("Error: Not Implemented"), false.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% PART 4: Ex. 8.3.1 %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (i) triangular_num(N, T) <-
% 		T is the Nth Triangular Number.
triangular_num(N, T) :-
    write("Error: Not Implemented"), false.


% (iii) reverse_between(I, J, K)
% 	K is an integer between J and I inclusive, J > I.
reverse_between(I, J, K) :-
    write("Error: Not Implemented"), false.


% (vi) min_list(List, Min)
%	Min is the minimum int in the List of integers.
min_list(List, Min) :-
    write("Error: Not Implemented"), false.


% (vii) length_iter(List, Len)
%	Len of list List using an iterative function.
length_iter(List, Len) :-
    write("Error: Not Implemented"), false.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% Unit Tests  %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(home_work_2).

    /* Test 1: Part 1::(ii) a. */
    test(adjacent) :- 
        adjacent(1, 2, [1, 2, 3]),
        not(adjacent(1, 2, [1, 3, 2])).

    /* Test 2: Part 1::(ii) b. */
    test(last) :- 
        last(9, [9, 1, 1, 9]),
        not(last(1, [9, 1, 1, 9])).

    /* Test 3: Part 1::(iii) */
    test(double) :-
        double([1, 2], [1, 1, 2, 2]),
        double([1, 2], [1, 1, 2, 2]),
        double([1, 2], [1, 2, 1, 2]),
        double([1, 2], [1, 2, 2, 1]),
        double([1, 2], [1, 2, 1, 2]),
        double([1, 2], [1, 2, 2, 1]),
        double([1, 2], [1, 1, 2, 2]),
        double([1, 2], [1, 1, 2, 2]),
        double([1, 2], [1, 2, 1, 2]),
        double([1, 2], [1, 2, 2, 1]),
        double([1, 2], [1, 2, 1, 2]),
        double([1, 2], [1, 2, 2, 1]),
        double([1, 2], [2, 1, 1, 2]),
        double([1, 2], [2, 1, 2, 1]),
        double([1, 2], [2, 1, 1, 2]),
        double([1, 2], [2, 1, 2, 1]),
        double([1, 2], [2, 2, 1, 1]),
        double([1, 2], [2, 2, 1, 1]),
        double([1, 2], [2, 1, 1, 2]),
        double([1, 2], [2, 1, 2, 1]),
        double([1, 2], [2, 1, 1, 2]),
        double([1, 2], [2, 1, 2, 1]),
        double([1, 2], [2, 2, 1, 1]),
        double([1, 2], [2, 2, 1, 1]),
        double([1, 2], [1, 2, 3, 4, 5, 6, 1, 2]).

    /* Test 4: Part 2::(i) */
    test(substitute) :- 
        substitute(0, 5, [0, 1, 0, 3, 0, 5, 0], [5, 1, 5, 3, 5, 5, 5]).

    /* Test 5: Part 2::(iii) */
    test(no_doubles) :- 
        no_doubles([1, 3, 2, 2, 3, 2], [1, 2, 3]).

    /* Test 6: Part 2::(v) */
    test(mergesort) :- 
        mergesort([1, 9, 8, 5, 3, 6, 7, 2, 4], [1, 2, 3, 4, 5, 6, 7, 8, 9]),
        mergesort([1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9]),
        mergesort([9, 8, 7, 6, 5, 4, 3, 2, 1], [1, 2, 3, 4, 5, 6, 7, 8, 9]).
    

    /* Test 7: Part 2::(vi)  */
    test(kth_largest) :-
        kth_largest([4, 1, 6, 2, 5, 3], 1, 6),
        kth_largest([5, 4, 6, 3, 1, 2], 2, 5),
        kth_largest([2, 1, 5, 3, 4, 6], 3, 4),
        kth_largest([4, 2, 6, 3, 5, 1], 4, 3),
        kth_largest([5, 3, 4, 2, 1, 6], 5, 2),
        kth_largest([1, 6, 4, 5, 3, 2], 6, 1).
        
    /* Test 8: Part 2::(vii) */
    test(better_poker_hand) :- 
        better_poker_hand(
            [card(diamond, 10), card(club, 2), card(heart, ace), card(spade, ace), card(diamond, king)], 
            [card(heart, 2), card(heart, 3), card(heart, 4), card(heart, 5), card(heart, 6)], 
            [card(heart, 2), card(heart, 3), card(heart, 4), card(heart, 5), card(heart, 6)]
        ),
        better_poker_hand(
            [card(heart, 5), card(spade, queen), card(heart, king), card(diamond, king), card(spade, 7)],
            [card(diamond, 5), card(spade, king), card(diamond, queen), card(club, queen), card(club, 7)],
            [card(heart, 5), card(spade, queen), card(heart, king), card(diamond, king), card(spade, 7)]
        ),
        better_poker_hand(
            [card(heart, 5), card(spade, queen), card(heart, ace), card(diamond, 4), card(spade, 7)],
            [card(diamond, 5), card(spade, king), card(diamond, queen), card(club, jack), card(club, 7)],
            [card(heart, 5), card(spade, queen), card(heart, ace), card(diamond, 4), card(spade, 7)]
        ).

    /* Test 9: Part 3::(i) */
    test(sumtree) :- 
        % sum of all numbers in tree, the numbers being
        %    from 1 to 10.
        once(dec_peano(10, TenPeano)),
        once(rangedec(TenPeano, OneToTenPeano)),
        once(inslist(OneToTenPeano, node, New)),
        once(sumtree(New, FiftyFivePeano)),
        once(peano_dec(FiftyFivePeano, 55)),
        % sum of numbers from 6 to 10
        %     limited list generation method.
        once(dec_peano(6, SixPeano)),
        once(rangedec(TenPeano, SixPeano, TenToSixPeano)),
        once(inslist(TenToSixPeano, node, Tree2)),
        once(sumtree(Tree2, FortyPeano)),
        once(peano_dec(FortyPeano, 40)).

    /* Test 10: Part 3::(ii) */
    test(delete) :- 
        % sum of numbers from 6 to 10
        %     delete 1 to 5 from tree.
        once(dec_peano(10, TenPeano)),
        once(rangedec(TenPeano, OneToTenPeano)),
        once(inslist(OneToTenPeano, node, Tree1)),
        once(dec_peano(5, FivePeano)),
        once(rangedec(FivePeano, OneToFivePeano)),
        once(dellist(OneToFivePeano, Tree1, Tree2)),
        once(sumtree(Tree2, FortyPeano)),
        once(peano_dec(FortyPeano, 40)),
        % sum of numbers from 1 to 9 (delete 10 from tree)
        once(delete(TenPeano, Tree1, Tree3)),
        once(sumtree(Tree3, FortyFivePeano)),
        once(peano_dec(FortyFivePeano, 45)).


    /* Test 11: Part 4::(i) */
    test(triangular_num) :-
        triangular_num(0, 0),
        triangular_num(1, 1),
        triangular_num(2, 3),
        triangular_num(3, 6),
        triangular_num(11, 66),
        triangular_num(8, X),
        triangular_num(X, 666).

    /* Test 12: Part 4::(iii) */
    test(reverse_between) :- 
        findall(X, reverse_between(1, 4, X), Ls),
        msort(Ls, [1, 2, 3, 4]),
        findall(X, reverse_between(10, 24, X), Ls2),
        msort(Ls2, [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]).

    /* Test 13: Part 4::(vi) */
    test(min_list) :- 
        min_list([10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24], 10),
        min_list([6, 3, 4, 10, 5, 8, 9, 7], 3).

    /* Test 14: Part 4::(vii) */
    test(length_iter) :- 
        length_iter([6, 3, 4, 10, 5, 8, 9, 7], 8),
        length_iter([6, 4, 10, 5, 8], 5),
        length_iter([], 0).

    
:- end_tests(home_work_2).