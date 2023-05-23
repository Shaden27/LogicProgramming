/*
 * Name:        <Shreepad> <Divekar> (smd220000)
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
The key difference is every sublist is s subsequence but every subsequence is not a sublist. 
For example, if Xs = [1,3] and Ys = [1,2,3,4], then Xs is a subsequence of Ys (since [1,3] appears in Ys in the same order), 
but Xs is not a prefix or a sublist of Ys (since it is not contiguous). 
*/




% (ii) Write recursive programs for adjacent and last that
% are semantically equiv. to the ones defined on pg. 62,
% WITHOUT using the append predicate.
%adjacent(X, Y, Zs) :-
%    write("Error: Not Implemented"), false.

%last(X, Xs) :-
%    write("Error: Not Implemented"), false.

adjacent(X,Y,[X,Y|Zs]).
adjacent(X,Y,[Z|Zs]):-adjacent(X,Y,Zs).

last(X,[X]).
last(X,[Y|Xs]):-last(X,Xs).


% (iii) double(List, ListList) s.t.
% Every element in List appears twice in ListList
%     i.e. all permutations are admissible
%double(List, ListList) :-
%    write("Error: Not Implemented"), false.

double([], _).
double([X|Xs], ListList) :-
    select(X, ListList, Rest),
    select(X, Rest, Rest2),
    double(Xs, Rest2).


% (iv) The size of proof tree as a function of the size 
% of the input list for prog. 3.16a and prog 3.16b.
% The exact size or an asymptotic bound, either is acceptable.

/*
3.16a
Recurrence Relation: P(n) = P(n-1) + O(n), n > 0
Complexity: O(n^2)
3.16b
Complexity: O(n)
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% PART 2: Ex. 3.3.1 %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% (i) substitute(X, Y, L1, L2) <-
%	  	L2 is a result of substituting all occurences of X in L1 with Y
% Suggestion: name your implementation substitute_helper, and call
%   it within substitute.

%substitute(X, Y, L1, L2) :-
%   write("Error: Not Implemented"), false.
substitute(X,Y,[],[]).
substitute(X,Y,[X|Xs],[Y|Ys]):-substitute(X,Y,Xs,Ys).
substitute(X,Y,[Z|Xs],[Z|Ys]):-X\=Z, substitute(X,Y,Xs,Ys).


% (ii) What is the meaning of the given variant of select?
sel(X, [X|Xs], Xs).
sel(X, [Y|Ys], [Y|Zs]) :- X \= Y, sel(X, Ys, Zs).

/*
The first clause matches the case when the first element of the list is X. It returns the tail of the list (i.e., Xs) as the result. 
This clause essentially removes the first occurrence of X from the list and returns the modified list.
The second clause applies when the first element of the list is not X. It first checks if X is not equal to Y (i.e., X/= Y), and if that is true,
it recursively calls the "select" predicate on the tail of the list (i.e., Ys) to look for X.
*/



% (iii) no_doubles(L1, L2) <- 
% 	where L2 is a result of removing all duplicates from L1
%no_doubles(L1, L2) :-
%    write("Error: Not Implemented"), false.

no_doubles(List, Result) :- sort(List, SortedList), no_doubles_helper(SortedList, Result).
no_doubles_helper([], []).
no_doubles_helper([H|T], [H|P]) :-
    not(member(H, T)),
    no_doubles_helper(T, P).
no_doubles_helper([H|T], P) :-
    member(H, T),
    no_doubles_helper(T, P).


% (v) mergesort(Xs, Ys) <-
%	Ys is an ordered permutation of the list Xs
%mergesort(Xs, Ys) :-
%    write("Error: Not Implemented"), false.

mergesort([], []).
mergesort([X], [X]).


mergesort([Odd,Even|Xs], Ys):-
    split([Odd,Even|Xs], Odds, Evens),
    mergesort(Odds, Os),
    mergesort(Evens, Es),
    merge(Os, Es, Ys).

split([], [], []).
split([X|Xs], [X|Os], Es):-split(Xs, Es, Os).

merge([], Ys, Ys).
merge([X|Xs], [], [X|Xs]).
merge([X|Xs], [Y|Ys], [X|Zs]):-X<Y,  merge(Xs, [Y|Ys], Zs).
merge([X|Xs], [Y|Ys], [Y|Zs]):-X>=Y, merge([X|Xs], Ys, Zs).




% (vi) kth_largest(Xs, K, V) <-
%	Finds the kth largest element of the list Xs.
%kth_largest(Xs, K, V) :-
%    write("Error: Not Implemented"), false.

% I have used pre defined funcions but all of these functions were implemented in the lectures
kth_largest(Xs, K, V) :-
    length(Xs, N),              % Calculate Length of List
    N >= K, K > 0,              % Check constraints for given K
    sort(Xs, Sorted),           % Sort List
    reverse(Sorted, Reversed),  % Reverse List
    nth1(K, Reversed, V).       % Find kth largest


% (vii) better_poker_hand(Hand1, Hand2, Hand)
%	Where Hand is the better of the two hands.
% Conventions:
%     card(Suit, Value)
%     Suit ∈ {heart, diamond, spade, club}
%     Value ∈ {2,3,4,5,6,7,8,9,10,jack,queen,king,ace}
%     House Rule: no baby straight/5-high straight i.e. A 2 3 4 5
%better_poker_hand(Hand1, Hand2, Hand) :-
%    write("Error: Not Implemented"), false.
% Define the values of the cards
card_value(ace,14).
card_value(king,13).
card_value(queen,12).
card_value(jack,11).
card_value(10,10).
card_value(9,9).
card_value(8,8).
card_value(7,7).
card_value(6,6).
card_value(5,5).
card_value(4,4).
card_value(3,3).
card_value(2,2).

% Define the suits of the cards
card_suit(spade).
card_suit(heart).
card_suit(club).
card_suit(diamond).


valid_hand(Hand) :-
    % A valid hand must have exactly 5 cards
    length(Hand, 5),
    % Check that each card in the hand is valid and unique
    maplist(valid_card, Hand),
    % Check that there are no duplicate cards in the hand
    sort(Hand, SortedHand),
    length(Hand, HandLength),
    length(SortedHand, SortedHandLength),
    HandLength = SortedHandLength.

% Define a valid card
valid_card(card(Suit, Value)) :-
    % A valid card must have a valid suit and value
    member(Suit, [club, diamond, heart, spade]),
    member(Value, [2,3,4,5,6,7,8,9,10,jack,queen,king,ace]).

% Define the rank of the hand
% A hand is ranked as follows:
%   1. Straight Flush
%   2. Four of a Kind
%   3. Full House
%   4. Flush
%   5. Straight
%   6. Three of a Kind
%   7. Two Pair
%   8. Pair
%   9. High Card
valid_and_sort([card(S1,VV1),card(S2,VV2),card(S3,VV3),card(S4,VV4),card(S5,VV5)], Rank) :- 
    valid_hand([card(S1,VV1),card(S2,VV2),card(S3,VV3),card(S4,VV4),card(S5,VV5)]),
    card_value(VV1,V1),
	card_value(VV2,V2),
	card_value(VV3,V3),
	card_value(VV4,V4),
	card_value(VV5,V5),
    msort([V1,V2,V3,V4,V5], [S1V,S2V,S3V,S4V,S5V]), 
    hand_rank(S1V, S2V, S3V, S4V, S5V, S1, S2, S3, S4, S5, Rank).

hand_rank(S1V, S2V, S3V, S4V, S5V, S1, S2, S3, S4, S5, Rank) :- 
    % Check for straight flush
    (S1 = S2, S2 = S3, S3 = S4, S4 = S5, S2V is S1V + 1, S3V is S2V + 1, S4V is S3V + 1, S5V is S4V + 1) ->
        Rank = 1
    % Check for four of a kind
    ; (S1V = S2V, S2V = S3V, S3V = S4V, S4V \= S5V) ->
        Rank = 2
    ; (S1V \= S2V, S2V = S3V, S3V = S4V, S4V = S5V) ->
        Rank = 2
    
    % Check for full house
    ;(S1V = S2V, S2V = S3V, S4V = S5V, S1V \= S4V) ->
        Rank = 3
    ;(S1V = S2V, S3V = S4V, S4V = S5V, S2V \= S3V) ->
        Rank = 3
    
    % Check for straight
    ; S2V is S1V + 1, S3V is S2V + 1, S4V is S3V + 1, S5V is S4V + 1 ->
        Rank = 4
    
    % Check for flush
    ; S1 = S2, S2 = S3, S3 = S4, S4 = S5 ->
        Rank = 5
    
    % Check for three of a kind
    ; (S1V = S2V, S2V = S3V) ->
        Rank = 6
    ; (S2V = S3V, S3V = S4V) ->
        Rank = 6
    ; (S3V = S4V, S4V = S5V) ->
        Rank = 6
    % Check for two pair
    ; (S1V = S2V, S3V = S4V) ->
        Rank = 7
    ; (S1V = S2V, S4V = S5V) ->
        Rank = 7
    ; (S2V = S3V, S4V = S5V) ->
        Rank = 7
    % Check for pair
    ; (S1V = S2V) ->
        Rank = 8
    ; (S2V = S3V) ->
        Rank = 8
    ; (S3V = S4V) ->
        Rank = 8
    ; (S4V = S5V) ->
        Rank = 8
    % Otherwise, it's a high card hand
    ; true ->
        Rank = 9.


better_poker_hand(Hand1,Hand2,Hand) :- 
    
    
    valid_and_sort(Hand1, R1),
    valid_and_sort(Hand2, R2),
    (R1 > R2) ->  Hand = Hand2;
    valid_and_sort(Hand1, R1), 
    valid_and_sort(Hand2, R2), 
    (R1 < R2) ->  Hand = Hand1;
    valid_and_sort(Hand1, R1), 
    valid_and_sort(Hand2, R2),
    (R1 = R2) ->  compare_helper(Hand1,Hand2, R1, Hand3),reduce_down(Hand3, Hand4), Hand = Hand4.

compare_helper([card(Hand1S1,Hand1VV1),card(Hand1S2,Hand1VV2),card(Hand1S3,Hand1VV3),card(Hand1S4,Hand1VV4),card(Hand1S5,Hand1VV5)],
               [card(Hand2S1,Hand2VV1),card(Hand2S2,Hand2VV2),card(Hand2S3,Hand2VV3),card(Hand2S4,Hand2VV4),card(Hand2S5,Hand2VV5)],
               R1, Hand3) :-
    card_value(Hand1VV1,Hand1V1),
	card_value(Hand1VV2,Hand1V2),
	card_value(Hand1VV3,Hand1V3),
	card_value(Hand1VV4,Hand1V4),
	card_value(Hand1VV5,Hand1V5),
    msort([Hand1V1,Hand1V2,Hand1V3,Hand1V4,Hand1V5], [Hand1S1V,Hand1S2V,Hand1S3V,Hand1S4V,Hand1S5V]),
    card_value(Hand2VV1,Hand2V1),
	card_value(Hand2VV2,Hand2V2),
	card_value(Hand2VV3,Hand2V3),
	card_value(Hand2VV4,Hand2V4),
	card_value(Hand2VV5,Hand2V5),
    msort([Hand2V1,Hand2V2,Hand2V3,Hand2V4,Hand2V5], [Hand2S1V,Hand2S2V,Hand2S3V,Hand2S4V,Hand2S5V]),
    compare([card(Hand1S1,Hand1V1),card(Hand1S2,Hand1V2),card(Hand1S3,Hand1V3),card(Hand1S4,Hand1V4),card(Hand1S5,Hand1V5)],
            [card(Hand2S1,Hand2V1),card(Hand2S2,Hand2V2),card(Hand2S3,Hand2V3),card(Hand2S4,Hand2V4),card(Hand2S5,Hand2V5)],
            Hand1S1V,Hand1S2V,Hand1S3V,Hand1S4V,Hand1S5V,
            Hand2S1V,Hand2S2V,Hand2S3V,Hand2S4V,Hand2S5V,
            R1, Hand3).

reduce_down([card(S1,VV1),card(S2,VV2),card(S3,VV3),card(S4,VV4),card(S5,VV5)], Hand4) :-
    card_value(V1,VV1),
	card_value(V2,VV2),
	card_value(V3,VV3),
	card_value(V4,VV4),
	card_value(V5,VV5),
    Hand4 = [card(S1,V1),card(S2,V2),card(S3,V3),card(S4,V4),card(S5,V5)].
% get the distinct elements of a sorted list
distinct([], []).
distinct([X], [X]).
distinct([X, Y|Xs], [X|Rest]) :-
    % X is distinct, add it to the distinct list
    X \= Y,
    distinct([Y|Xs], Rest).
distinct([X, X|Xs], Rest) :-
    % X is a duplicate, skip it and continue with the rest of the list
    distinct([X|Xs], Rest).

% get the duplicate elements of a sorted list
duplicates([], []).
duplicates([_], []).
duplicates([X, X|Xs], [X|Rest]) :-
    % X is a duplicate, add it to the duplicates list
    duplicates([X|Xs], Rest).
duplicates([X, Y|Xs], Rest) :-
    % X is distinct, continue with the rest of the list
    X \= Y,
    duplicates([Y|Xs], Rest).

% get the difference between two lists
list_difference([], _, []).
list_difference(_, [], []).
list_difference([X|Xs], Ys, Diff) :-
    member(X, Ys),
    % X is in Ys, skip it and continue with the rest of the list
    list_difference(Xs, Ys, Diff).
list_difference([X|Xs], Ys, [X|Diff]) :-
    % X is not in Ys, add it to the difference list and continue with the rest of the list
    \+ member(X, Ys),
    list_difference(Xs, Ys, Diff).

max([X], X).
max([X,Y|Xs], Max) :-
    X >= Y,
    max([X|Xs], Max).
max([X,Y|Xs], Max) :-
    X < Y,
    max([Y|Xs], Max).

min([X], X).
min([X,Y|Xs], Min) :-
    X =< Y,
    min([X|Xs], Min).
min([X,Y|Xs], Min) :-
    X > Y,
    min([Y|Xs], Min).

compare_two_pair(L1,L2,Sol) :- 
duplicates(L1, Dup1), distinct(L1, Dist1), list_difference(Dist1, Dup1, Diff1), max(Dup1, Max1), min(Dup1,Min1), max(Diff1,MDiff1),
duplicates(L2, Dup2), distinct(L2, Dist2), list_difference(Dist2, Dup2, Diff2), max(Dup2, Max2), min(Dup2,Min2), max(Diff2,MDiff2),
compare_two_pair_helper(Max1, Min1, MDiff1,Max2, Min2, MDiff2, Sol).

compare_two_pair_helper(Max1, Min1, MDiff1,Max2, Min2, MDiff2, Sol) :- 
    (Max1 > Max2) ->  Sol = 1;
    (Max1 < Max2) ->  Sol = 2;
    (Max1 = Max2) ->  
    (   
    (Min1 > Min2) ->  Sol = 1;
    (Min1 < Min2) ->  Sol = 2;
    (Min1 = Min2) ->  (   (MDiff1 > MDiff2) ->   Sol = 1;
    (MDiff1 < MDiff2) ->   Sol = 2)).

compare_one_pair(L1,L2,Sol) :- 
    duplicates(L1, Dup1), distinct(L1, Dist1), list_difference(Dist1, Dup1, Diff1), max(Dup1, Max1), max(Diff1,MDiff1),
	duplicates(L2, Dup2), distinct(L2, Dist2), list_difference(Dist2, Dup2, Diff2), max(Dup2, Max2), max(Diff2,MDiff2),
    compare_one_pair_helper(Max1, MDiff1,Max2, MDiff2, Sol).

compare_one_pair_helper(Max1, MDiff1,Max2, MDiff2, Sol) :-
    (Max1 > Max2) ->  Sol = 1;
    (Max1 < Max2) ->  Sol = 2;
    (Max1 = Max2) ->  
    (   
    (MDiff1 > MDiff2) ->  Sol = 1;
    (MDiff1 < MDiff2) ->  Sol = 2).

compare(Hand1, Hand2, Hand1S1V,Hand1S2V,Hand1S3V,Hand1S4V,Hand1S5V,
            Hand2S1V,Hand2S2V,Hand2S3V,Hand2S4V,Hand2S5V,
            R1, Hand3) :- 
    % Straight Flush
    (R1 = 1, Hand1S1V > Hand2S1V) ->  Hand3 = Hand1;
    (R1 = 1, Hand1S1V < Hand2S1V) ->  Hand3 = Hand2;
    % 4 of a kind
    (R1 = 2, Hand1S3V > Hand2S3V) ->  Hand3 = Hand1;
    (R1 = 2, Hand1S3V < Hand2S3V) ->  Hand3 = Hand2;
    % Full House
    (R1 = 3, Hand1S3V > Hand2S3V) ->  Hand3 = Hand1;
    (R1 = 3, Hand1S3V < Hand2S3V) ->  Hand3 = Hand2;
    %Straight
    (R1 = 4, Hand1S3V > Hand2S3V) ->  Hand3 = Hand1;
    (R1 = 4, Hand1S3V < Hand2S3V) ->  Hand3 = Hand2;
    % 3 of a kind
    (R1 = 6, Hand1S5V > Hand2S3V) ->  Hand3 = Hand1;
    (R1 = 6, Hand1S3V < Hand2S3V) ->  Hand3 = Hand2;
    % 2 pair
    (R1 = 7, compare_two_pair([Hand1S1V,Hand1S2V,Hand1S3V,Hand1S4V,Hand1S5V],
                              [Hand2S1V,Hand2S2V,Hand2S3V,Hand2S4V,Hand2S5V]
                              ,Sol), Sol = 1) ->   Hand3 = Hand1;
    (R1 = 7, compare_two_pair([Hand1S1V,Hand1S2V,Hand1S3V,Hand1S4V,Hand1S5V],
                              [Hand2S1V,Hand2S2V,Hand2S3V,Hand2S4V,Hand2S5V]
    						  ,Sol), Sol = 2) ->  Hand3 = Hand2;
    % 1 pair
    (R1 = 8, compare_two_pair([Hand1S1V,Hand1S2V,Hand1S3V,Hand1S4V,Hand1S5V],
                              [Hand2S1V,Hand2S2V,Hand2S3V,Hand2S4V,Hand2S5V]
                              ,Sol), Sol = 1) ->   Hand3 = Hand1;
    (R1 = 8, compare_two_pair([Hand1S1V,Hand1S2V,Hand1S3V,Hand1S4V,Hand1S5V],
                              [Hand2S1V,Hand2S2V,Hand2S3V,Hand2S4V,Hand2S5V]
    						  ,Sol), Sol = 2) ->  Hand3 = Hand2; 
    % High Card
    (R1 = 9, Hand1S5V > Hand2S5V) ->  Hand3 = Hand1;
    (R1 = 9, Hand1S5V < Hand2S5V) ->  Hand3 = Hand2.
    

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
%sumtree(T, N) :-
%    write("Error: Not Implemented"), false.
sumtree(node,0).
sumtree(node(X,L,R),N) :- 
    sumtree(L,N1), sumtree(R,N2),
    add(N1,N2,N0), add(X,N0,N).

% (ii) delete(E, T, Tn): delete the element E from SBT T to obtain SBT Tn.
%delete(E, T, Tn) :-
%    write("Error: Not Implemented"), false.
delete(E, node(E, L, node), L).
delete(E, node(E, node, R), R).
delete(E, node(E, L, R), node(M, L, R)) :- get_max(L, M), delete(M, L, _).
delete(E, node(V, L, R), node(V, L2, R)) :- lt(E,V), delete(E, L, L2).
delete(E, node(V, L, R), node(V, L, R2)) :- gt(E,V), delete(E, R, R2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% PART 4: Ex. 8.3.1 %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (i) triangular_num(N, T) <-
% 		T is the Nth Triangular Number.
%triangular_num(N, T) :-
%    write("Error: Not Implemented"), false.
triangular_num(0,0).
triangular_num(N,T):-N>0, N1 is N - 1, triangular_num(N1,T1), T is T1+N.

% (iii) reverse_between(I, J, K)
% 	K is an integer between J and I inclusive, J > I.
%reverse_between(I, J, K) :-
%    write("Error: Not Implemented"), false.
reverse_between(I,J,J):-J>=I.
reverse_between(I,J,K):-J>I, J1 is J - 1, reverse_between(I,J1,K).

% (vi) min_list(List, Min)
%	Min is the minimum int in the List of integers.
%min_list(List, Min) :-
%    write("Error: Not Implemented"), false.
min_list([],Min,Min).
min_list([I|Is],Temp,Min):-min_1(I,Temp,Temp1), min_list(Is,Temp1,Min).

min_1(I,J,I):-J >= I, !.
min_1(I,J,J):-J < I.

% (vii) length_iter(List, Len)
%	Len of list List using an iterative function.
%length_iter(List, Len) :-
%    write("Error: Not Implemented"), false.
length_iter(Xs,L):-length_iter(Xs,0,L).

length_iter([],L,L).
length_iter([X|Xs],L0,L):-L1 is L0+1, length_iter(Xs,L1,L).


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