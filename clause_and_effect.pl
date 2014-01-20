% Worksheet 16

my_transpose([[]|_],[]).
my_transpose(R, [H|C]) :-
  chopcol(R, H, T),
  my_transpose(T, C).

chopcol([],[],[]).
chopcol([[H|T]|Cs], [H|Hs], [T|Ts]) :-
  chopcol(Cs, Hs, Ts).


% Worksheet 17

enum(A,B) :-
  enum(A, 1, B).
enum([], _, []).
enum([A|As], N, [n(A,N)|Bs]) :-
  N2 is N + 1,
  enum(As, N2, Bs).

