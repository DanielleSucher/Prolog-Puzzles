:- use_module(library(clpfd)).

% The eight queens puzzle is the problem of placing eight chess queens on
% an 8×8 chessboard so that no two queens attack each other.
% Thus, a solution requires that no two queens share the same row, column, or diagonal.

% solution adapted from http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_11.html
% (mostly just to be more readable to me, honestly)

eight_queens(Queens) :-
  check_horizontals_and_verticals(Queens),
  check_diagonals([1,2,3,4,5,6,7,8], Queens, Sums, Differences),
  all_distinct(Sums),
  all_distinct(Differences).

check_horizontals_and_verticals(Queens) :-
  length(Queens, 8),
  Queens ins 1..8,
  all_distinct(Queens),
  label(Queens).

check_diagonals([Row|Rows], [Column|Columns], [Sum|Sums], [Diff|Diffs]) :-
  Sum is Row + Column,
  Diff is Row - Column,
  check_diagonals(Rows, Columns, Sums, Diffs).
check_diagonals([],[],[],[]).
