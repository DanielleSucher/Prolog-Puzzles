% Puzzles from:
% http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/
set_prolog_flag(toplevel_print_options, [quoted(true), portray(true), max_depth(100), spacing(next_argument)]).

:- use_module(library(clpfd)).

% Section 1: Working with Prolog lists

% P01 (*) Find the last element of a list.
  % Example:
  % ?- my_last(X,[a,b,c,d]).
  % X = d
my_last(Last, [Last]).
my_last(Last, [_|T]) :- my_last(Last, T).


% P02 (*) Find the last but one element of a list.
second_to_last(X, [X,_]).
second_to_last(X, [_|T]) :- second_to_last(X, T).


% P03 (*) Find the K'th element of a list.
  % The first element in the list is number 1.
  % Example:
  % ?- element_at (X,[a,b,c,d,e],3).
  % X = c
element_at(X, List, Position) :-
  element_at(X, List, Position, 1).
element_at(X, [X|_], Position, Position) :-
  !.
element_at(X, [_|T], Position, HeadIndex) :-
  NewHeadIndex is HeadIndex + 1,
  element_at(X, T, Position, NewHeadIndex).


% P04 (*) Find the number of elements of a list.
my_count([], 0).
my_count([_|T], Count) :-
  my_count(T, NewCount),
  Count is NewCount + 1.


% P05 (*) Reverse a list.
my_reverse(X, Y) :- my_reverse(X, Y, []).
my_reverse([], X, X) :- !.
my_reverse([H|T], Reversed, Acc) :-
  my_reverse(T, Reversed, [H|Acc]).


% P06 (*) Find out whether a list is a palindrome.
  % A palindrome can be read forward or backward; e.g. [x,a,m,a,x].
palindrome(List) :- my_reverse(List, List).


% P07 (**) Flatten a nested list structure.
  % Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

  % Example:
  % ?- my_flatten([a, [b, [c, d], e]], X).
  % X = [a, b, c, d, e]

  % Hint: Use the predefined predicates is_list/1 and append/3
 
% my working solution from before reading the official solution:
% my_flatten(NestedList, X) :-
  % my_flatten(NestedList, X, []).
% my_flatten([], X, Acc) :-
  % my_reverse(X, Acc).
% my_flatten([[]|T], X, Acc) :-
  % my_flatten(T, X, Acc).
% my_flatten([[H|T]|Rest], X, Acc) :-
  % my_flatten([H,T|Rest], X, Acc).
% my_flatten([H|T], X, Acc) :-
  % my_flatten(T, X, [H|Acc]).

% from the answers section, nicer:
my_flatten(X,[X]) :-
  \+ is_list(X).
my_flatten([],[]).
my_flatten([X|Xs],Zs) :- 
  my_flatten(X,Y),
  my_flatten(Xs,Ys),
  append(Y,Ys,Zs).


% P08 (**) Eliminate consecutive duplicates of list elements.
  % If a list contains repeated elements they should be replaced with a single copy of the element.
  % The order of the elements should not be changed.

  % Example:
  % ?- compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
  % X = [a,b,c,a,d,e]
 
% my working solution from before reading the official solution:
% compress(List, X) :-
  % compress(List, X, []).
% compress([], X, Acc) :-
  % my_reverse(X, Acc).
% compress([H|[]], X, Acc) :-
  % compress([], X, [H|Acc]).
% compress([H,H|T], X, Acc) :-
  % compress([H|T], X, Acc).
% compress([H1,H2|T], X, Acc) :-
  % compress([H2|T], X, [H1|Acc]).

% from the answers section, nicer:
compress([],[]).
compress([X],[X]).
compress([X,X|Xs],Zs) :-
  compress([X|Xs],Zs).
compress([X,Y|Ys],[X|Zs]) :-
  X \= Y,
  compress([Y|Ys],Zs).

% P09 (**) Pack consecutive duplicates of list elements into sublists.
  % If a list contains repeated elements they should be placed in separate sublists.

  % Example:
  % ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
  % X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
pack([H|T], X) :-
  pack(T, X, [H], []).
pack([], X, Sublist, Acc) :-
  my_reverse(X, [Sublist|Acc]).
pack([H|T], X, [H|Sublist], Acc) :-
  pack(T, X, [H,H|Sublist], Acc).
pack([H|T], X, [H2|Sublist], Acc) :-
  H \= H2,
  pack(T, X, [H], [[H2|Sublist]|Acc]).


% P10 (*) Run-length encoding of a list.
  % Use the result of problem P09 to implement the so-called run-length encoding data compression method.
  % Consecutive duplicates of elements are encoded as terms [N,E]
  % where N is the number of duplicates of the element E.

  % Example:
  % ?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
  % X = [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]
encode([], []).
encode([X], [[1, X]]) :-
  \+ is_list(X).
encode([X1, X2|Xs], [[1, X1]|Ys]) :-
  X1 \= X2,
  encode([X2|Xs], Ys).
encode([X, X|Xs], [[N, X]|Ys]) :-
  encode([X|Xs], [[N2, X]|Ys]),
  N is N2 + 1.


% P11 (*) Modified run-length encoding.
  % Modify the result of problem P10 in such a way that if an element has no
  % duplicates it is simply copied into the result list.
  % Only elements with duplicates are transferred as [N,E] terms.

  % Example:
  % ?- encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
  % X = [[4,a],b,[2,c],[2,a],d,[4,e]]

% from the answers section (because it helps clear up some conceptual stuff
% better for me than my original solution, which worked but not in a very
% logic-programming sort of way):
encode_modified(X, Y) :-
  encode(X, Encoded),
  strip(Encoded, Y).

strip([],[]).
strip([[1,X]|Ys], [X|Zs]) :-
  strip(Ys, Zs).
strip([[N,X]|Ys], [[N,X]|Zs]) :-
  N > 1,
  strip(Ys, Zs).


% P12 (**) Decode a run-length encoded list.
  % Given a run-length code list generated as specified in problem P11.
  % Construct its uncompressed version.
decode_rl([], []).
decode_rl([E|Xs],[E|Ys]) :-
  \+ is_list(E),
  decode_rl(Xs, Ys).
decode_rl([[1, E]|Xs], [E|Ys]) :-
  decode_rl(Xs, Ys).
decode_rl([[N, E]|Xs], [E|Ys]) :-
  N > 1,
  Count is N - 1,
  decode_rl([[Count, E]|Xs], Ys).


% P13 (**) Run-length encoding of a list (direct solution).
  % Implement the so-called run-length encoding data compression method directly.
  % I.e. don't explicitly create the sublists containing the duplicates, as in
  % problem P09, but only count them. As in problem P11, simplify the result
  % list by replacing the singleton terms [1,X] by X.

  % Example:
  % ?- encode_direct([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
  % X = [[4,a],b,[2,c],[2,a],d,[4,e]]

% This is straight out of the answers, because it turned out I hadn't
% read the question right before solving what I thought it was and then
% going to double-check my soltion:
encode_direct([],[]).
encode_direct([X|Xs],[Z|Zs]) :-
  count(X,Xs,Ys,1,Z),
  encode_direct(Ys,Zs).

% count(X,Xs,Ys,K,T) Ys is the list that remains from the list Xs
%    when all leading copies of X are removed. T is the term [N,X],
%    where N is K plus the number of X's that can be removed from Xs.
%    In the case of N=1, T is X, instead of the term [1,X].

count(X,[],[],1,X).
count(X,[],[],N,[N,X]) :-
  N > 1.
count(X,[Y|Ys],[Y|Ys],1,X) :-
  X \= Y.
count(X,[Y|Ys],[Y|Ys],N,[N,X]) :-
  N > 1,
  X \= Y.
count(X,[X|Xs],Ys,K,T) :-
  K1 is K + 1,
  count(X,Xs,Ys,K1,T).


% P14 (*) Duplicate the elements of a list.
  % Example:
  % ?- dupli([a,b,c,c,d],X).
  % X = [a,a,b,b,c,c,c,c,d,d]
dupli([],[]).
dupli([X|Xs],[X,X|Ys]) :-
  dupli(Xs, Ys).


% P15 (**) Duplicate the elements of a list a given number of times.
  % Example:
  % ?- dupli([a,b,c],3,X).
  % X = [a,a,a,b,b,b,c,c,c]

  % What are the results of the goal:
  % ?- dupli(X,3,Y).
dupli(Xs, N, Ys) :-
  dupli(Xs, N, N, Ys).
dupli([], N, N, []).
dupli([X|Xs], N, 1, [X|Ys]) :-
  dupli(Xs, N, N, Ys).
dupli([X|Xs], N, M, [X|Ys]) :-
  M > 1,
  M2 is M - 1,
  dupli([X|Xs], N, M2, Ys).


% P16 (**) Drop every N'th element from a list.
  % Example:
  % ?- drop([a,b,c,d,e,f,g,h,i,k],3,X).
  % X = [a,b,d,e,g,h,k]
drop(Xs, N, Ys) :-
  drop(Xs, N, N, Ys).
drop([], _, _, []).
drop([_|Xs], N, 1, Ys) :-
  drop(Xs, N, N, Ys).
drop([X|Xs], N, M, [X|Ys]) :-
  M > 1,
  M2 is M - 1,
  drop(Xs, N, M2, Ys).


% P17 (*) Split a list into two parts; the length of the first part is given.
  % Do not use any predefined predicates.

  % Example:
  % ?- split([a,b,c,d,e,f,g,h,i,k],3,L1,L2).
  % L1 = [a,b,c]
  % L2 = [d,e,f,g,h,i,k]
split(X, 0, [], X).
split([X|Xs], N, [X|L1], L2) :-
  N > 0,
  N1 is N - 1,
  split(Xs, N1, L1, L2).


% P18 (**) Extract a slice from a list.
  % Given two indices, I and K, the slice is the list containing the elements
  % between the I'th and K'th element of the original list (both limits
  % included). Start counting the elements with 1.

  % Example:
  % ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
  % X = [c,d,e,f,g]
slice([], _, _, []).
slice(X, I, K, Y) :-
  slice(X, I, K, Y, 1).
slice([X|_], _, K, [X], K).
slice([X|Xs], I, K, [X|Ys], Index) :-
  Index >= I, Index < K,
  Index2 is Index + 1,
  slice(Xs, I, K, Ys, Index2).
slice([_|Xs], I, K, Ys, Index) :-
  Index < I,
  Index2 is Index + 1,
  slice(Xs, I, K, Ys, Index2).

% better solution from answer page:
% slice([X|_],1,1,[X]).
% slice([X|Xs],1,K,[X|Ys]) :- K > 1, 
   % K1 is K - 1, slice(Xs,1,K1,Ys).
% slice([_|Xs],I,K,Ys) :- I > 1, 
   % I1 is I - 1, K1 is K - 1, slice(Xs,I1,K1,Ys).


% P19 (**) Rotate a list N places to the left.
  % Examples:
  % ?- rotate([a,b,c,d,e,f,g,h],3,X).
  % X = [d,e,f,g,h,a,b,c]

  % ?- rotate([a,b,c,d,e,f,g,h],-2,X).
  % X = [g,h,a,b,c,d,e,f]

  % Hint: Use the predefined predicates length/2 and append/3, as well as
  % the result of problem P17.
rotate([], _, []).
rotate(X, 0, X).
rotate(X, N, Y) :- N > 0,
  length(X, Length),
  N2 is N mod Length,
  split(X, N2, L1, L2),
  append(L2, L1, Y).
rotate(X, N, Y) :- N < 0,
  length(X, Length),
  N2 is N + (N mod Length),
  rotate(X, N2, Y).


% P20 (*) Remove the K'th element from a list.
  % Example:
  % ?- remove_at(X,[a,b,c,d],2,R).
  % X = b
  % R = [a,c,d]
remove_at(X, [X|Xs], 1, Xs).
remove_at(X, [H|T], N, [H|R]) :-
  N2 is N - 1,
  remove_at(X, T, N2, R).


% P21 (*) Insert an element at a given position into a list.
  % Example:
  % ?- insert_at(alfa,[a,b,c,d],2,L).
  % L = [a,alfa,b,c,d]
insert_at(X, List, 1, [X|List]).
insert_at(X, [H|T], N, [H|R]) :-
  N2 is N - 1,
  insert_at(X, T, N2, R).


% P22 (*) Create a list containing all integers within a given range.
  % Example:
  % ?- range(4,9,L).
  % L = [4,5,6,7,8,9]
range(Z, Z, [Z]).
range(A, Z, [A|L]) :-
  A2 is A + 1,
  range(A2, Z, L).


% P23 (**) Extract a given number of randomly selected elements from a list.
  % The selected items shall be put into a result list.
  % Example:
  % ?- rnd_select([a,b,c,d,e,f,g,h],3,L).
  % L = [e,d,a]

  % Hint: Use the built-in random number generator random/2 and the result of problem P20.
rnd_select(_, 0, []).
rnd_select(X, N, [Y|Ys]) :-
  rnd_remove_at(Y, X, Xs),
  N2 is N - 1,
  rnd_select(Xs, N2, Ys).

rnd_remove_at(Removed, Complete, Remainder) :-
  length(Complete, Length),
  random_between(1, Length, R),
  remove_at(Removed, Complete, R, Remainder).


% P24 (*) Lotto: Draw N different random numbers from the set 1..M.
  % The selected numbers shall be put into a result list.
  % Example:
  % ?- rnd_select(6,49,L).
  % L = [23,1,17,33,21,37]

  % Hint: Combine the solutions of problems P22 and P23.
lotto(N, M, Y) :-
  range(1, M, Range),
  rnd_select(Range, N, Y).


% P25 (*) Generate a random permutation of the elements of a list.
  % Example:
  % ?- rnd_permu([a,b,c,d,e,f],L).
  % L = [b,a,d,c,e,f]

  % Hint: Use the solution of problem P23.
rnd_permu(List, Randomized) :-
  length(List, Length),
  rnd_select(List, Length, Randomized).


% P26 (**) Generate the combinations of K distinct objects chosen from
% the N elements of a list
  % In how many ways can a committee of 3 be chosen from a group of 12 people?
  % We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the
  % well-known binomial coefficients). For pure mathematicians, this result may
  % be great. But we want to really generate all the possibilities (via backtracking).

  % Example:
  % ?- combination(3,[a,b,c,d,e,f],L).
  % L = [a,b,c] ;
  % L = [a,b,d] ;
  % L = [a,b,e] ;
  % ...
% my solution: 
% combination(_, [], []).
% combination(N, List, Combined) :-
  % length(Combined, N),
  % my_sublist(List, Combined),
  % my_all_different(Combined).

% my_sublist(_, []).
% my_sublist(Xs, [Y|Ys]) :-
  % member(Y, Xs),
  % my_sublist(Xs, Ys).

% my_all_different([_]).
% my_all_different([X,Y|T]) :-
  % X =\= Y,
  % my_all_different([X|T]),
  % my_all_different([Y|T]).

% from the answers section:
combination(0,_,[]).
combination(N,L,[X|Xs]) :- N > 0,
   el(X,L,R),
   N1 is N - 1,
   combination(N1,R,Xs).

el(X,[X|L],L).
el(X,[_|L],R) :- el(X,L,R).


% P27 (**) Group the elements of a set into disjoint subsets.
  % a) In how many ways can a group of 9 people work in 3 disjoint subgroups of
  % 2, 3 and 4 persons? Write a predicate that generates all the possibilities
  % via backtracking.

  % Example:
  % ?- group3([aldo,beat,carla,david,evi,flip,gary,hugo,ida],G1,G2,G3).
  % G1 = [aldo,beat], G2 = [carla,david,evi], G3 = [flip,gary,hugo,ida]
  % ...
 
  % b) Generalize the above predicate in a way that we can specify a list of
  % group sizes and the predicate will return a list of groups.

  % Example:
  % ?- group([aldo,beat,carla,david,evi,flip,gary,hugo,ida],[2,2,5],Gs).
  % Gs = [[aldo,beat],[carla,david],[evi,flip,gary,hugo,ida]]
  % ...

  % Note that we do not want permutations of the group members;
  % i.e. [[aldo,beat],...] is the same solution as [[beat,aldo],...].
  % However, we make a difference between [[aldo,beat],[carla,david],...]
  % and [[carla,david],[aldo,beat],...].

  % You may find more about this combinatorial problem in a good book on discrete
  % mathematics under the term "multinomial coefficients".
group3(L, G1, G2, G3) :-
  group(L, [2,3,4], [G1,G2,G3]).

group([], [], []).
group(L, [N|Ns], [G|Gs]) :-
  combination(N, L, G),
  select(G, L, R),
  group(R, Ns, Gs).


% P28 (**) Sorting a list of lists according to length of sublists
  % a) We suppose that a list (InList) contains elements that are lists
  % themselves. The objective is to sort the elements of InList according to
  % their length. E.g. short lists first, longer lists later, or vice versa.

  % Example:
  % ?- lsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
  % L = [[o], [d, e], [d, e], [m, n], [a, b, c], [f, g, h], [i, j, k, l]]
lsort([], []).
lsort([X], [X]).
lsort(X, [H1, H2|T]) :-
  permutation(X, [H1, H2|T]),
  length(H1, L1),
  length(H2, L2), L2 >= L1,
  select(H1, X, X2),
  lsort(X2, [H2|T]).

  % b) Again, we suppose that a list (InList) contains elements that are lists
  % themselves. But this time the objective is to sort the elements of InList
  % according to their length frequency; i.e. in the default, where sorting is
  % done ascendingly, lists with rare lengths are placed first, others with a
  % more frequent length come later.

  % Example:
  % ?- lfsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
  % L = [[i, j, k, l], [o], [a, b, c], [f, g, h], [d, e], [d, e], [m, n]]

  % Note that in the above example, the first two lists in the result L have
  % length 4 and 1, both lengths appear just once. The third and forth list
  % have length 3 which appears, there are two list of this length. And finally,
  % the last three lists have length 2. This is the most frequent length.
lfsort(X, Y) :-
  lsort(X, Sorted), !,
  lf_encode(Sorted, Encoded),
  sort_encoded(Encoded, Ns),
  lfsort(X, Ns, Y).
lfsort([], [], []).
lfsort(X, [[0, _]|Ns], T) :-
  lfsort(X, Ns, T).
lfsort(X, [[N, L]|Ns], [H|T]) :-
  N > 0,
  N2 is N - 1,
  length(H, L),
  select(H, X, X2),
  lfsort(X2, [[N2, L]|Ns], T).

sort_encoded([], []).
sort_encoded([X], [X]).
sort_encoded(X, [[N1, L1], [N2, L2]|Ys]) :-
  permutation(X, [[N1, L1], [N2, L2]|Ys]),
  N1 =< N2,
  select([N1, L1], X, X2),
  sort_encoded(X2, [[N2, L2]|Ys]).

lf_encode([X], [[1, L]]) :-
  length(X, L).
lf_encode([X1, X2|Xs], [[1, L1]|Ys]) :-
  length(X1, L1),
  length(X2, L2),
  L1 =\= L2,
  lf_encode([X2|Xs], Ys).
lf_encode([X1, X2|Xs], [[N, L]|Ys]) :-
  length(X1, L), length(X2, L),
  lf_encode([X2|Xs], [[N2, L]|Ys]),
  N is N2 + 1.

