% Puzzles from:
% http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/

:- use_module(library(clpfd)).

% Section 2: Arithmetic

% P31 (**) Determine whether a given integer number is prime.
  % Example:
  % ?- is_prime(7).
  % Yes
is_prime(2).
is_prime(N) :-
  (N mod 2) =\= 0,
  is_prime(N, 3).
is_prime(N, F) :- F >= N.
is_prime(N, F) :- F < N,
  (N mod F) =\= 0,
  F2 is F + 1,
  is_prime(N, F2).


% P32 (**) Determine the greatest common divisor of two positive integer numbers.
  % Use Euclid's algorithm.
  % Example:
  % ?- gcd(36, 63, G).
  % G = 9
  % Define gcd as an arithmetic function; so you can use it like this:
  % ?- G is gcd(36,63).
  % G = 9
gcd(A, A, A).
gcd(A, B, G) :- A > B,
  C is A - B,
  gcd(C, B, G).
gcd(A, B, G) :- A < B,
  C is B - A,
  gcd(A, C, G).


% P33 (*) Determine whether two positive integer numbers are coprime.
  % Two numbers are coprime if their greatest common divisor equals 1.
  % Example:
  % ?- coprime(35, 64).
  % Yes
coprime(A, B) :- gcd(A, B, 1).


% P34 (**) Calculate Euler's totient function phi(m).
  % Euler's so-called totient function phi(m) is defined as the number of
  % positive integers r (1 <= r < m) that are coprime to m.
  % Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

  % ?- Phi is totient_phi(10).
  % Phi = 4

  % Find out what the value of phi(m) is if m is a prime number. Euler's totient
  % function plays an important role in one of the most widely used public key
  % cryptography methods (RSA). In this exercise you should use the most
  % primitive method to calculate this function (there are smarter ways that we
  % shall discuss later).
totient_phi(1, 1).
totient_phi(M, P) :-
  M2 is M - 1,
  tphi(M, M2, P).
tphi(_, 0, 0).
tphi(M, R, P) :-
  coprime(M, R),
  R2 is R - 1,
  tphi(M, R2, P2),
  P is P2 + 1.
tphi(M, R, P) :-
  \+ coprime(M, R),
  R2 is R - 1,
  tphi(M, R2, P).


% P35 (**) Determine the prime factors of a given positive integer.
  % Construct a flat list containing the prime factors in ascending order.
  % Example:
  % ?- prime_factors(315, L).
  % L = [3,3,5,7]
prime_factors(X, []) :- X =< 3.
prime_factors(N, L) :-
  prime_factors(N, 2, L).
prime_factors(N, F, []) :- F > N.
prime_factors(N, F, [F|Fs]) :- F =< N,
  N2 is N/F,
  integer(N2),
  is_prime(F),
  prime_factors(N2, F, Fs).
prime_factors(N, F, L) :- F =< N,
  0 =\= N mod F,
  F2 is F + 1,
  prime_factors(N, F2, L).
prime_factors(N, F, L) :- F =< N,
  \+ is_prime(F),
  F2 is F + 1,
  prime_factors(N, F2, L).


% P36 (**) Determine the prime factors of a given positive integer (2).
  % Construct a list containing the prime factors and their multiplicity.
  % Example:
  % ?- prime_factors_mult(315, L).
  % L = [[3,2],[5,1],[7,1]]
  % Hint: The problem is similar to problem P13.

:- ensure_loaded(lists).

prime_factors_mult(X, Y) :-
  prime_factors(X, X1),
  encode(X1, X2),
  swap(X2, Y).

swap([],[]).
swap([[A,B]|Xs], [[B,A]|Ys]) :- swap(Xs, Ys).


% P37 (**) Calculate Euler's totient function phi(m) (improved).
  % See problem P34 for the definition of Euler's totient function. If the list
  % of the prime factors of a number m is known in the form of problem P36 then
  % the function phi(m) can be efficiently calculated as follows: Let [[p1,m1],
  % [p2,m2],[p3,m3],...] be the list of prime factors (and their multiplicities)
  % of a given number m. Then phi(m) can be calculated with the following formula:
  %
  % phi(m) = (p1 - 1) * p1**(m1 - 1) * (p2 - 1) * p2**(m2 - 1) * (p3 - 1) * p3**(m3 - 1) * ...

  % Note that a**b stands for the b'th power of a.
mod_tot_phi(X, P) :-
  prime_factors_mult(X, Fs),
  mod_tphi(Fs, P).
mod_tphi([], 1).
mod_tphi([[F, M]|Fs], P) :-
  E is (F - 1) * F**(M - 1),
  mod_tphi(Fs, P1),
  P is P1 * E.


% P38 (*) Compare the two methods of calculating Euler's totient function.
  % Use the solutions of problems P34 and P37 to compare the algorithms. Take
  % the number of logical inferences as a measure for efficiency. Try to
  % calculate phi(10090) as an example.

% My note: mod_tot_phi (P37) was noticably faster.

% From the answers:
totient_test(N) :-
   write('totient_phi (P34):'),
   time(totient_phi(N,Phi1)),
   write('result = '), write(Phi1), nl,
   write('mod_tot_phi (P37):'),
   time(mod_tot_phi(N,Phi2)),
   write('result = '), write(Phi2), nl.


% P39 (*) A list of prime numbers.
  % Given a range of integers by its lower and upper limit, construct a list of
  % all prime numbers in that range.
primes(Lower, Upper, Primes) :-
  primes(Lower, Upper, Upper, Primes).
primes(Lower, _, X, []) :- X < Lower.
primes(Lower, Upper, P, [P|Ps]) :-
  is_prime(P),
  P2 is P - 1,
  primes(Lower, Upper, P2, Ps).
primes(Lower, Upper, P, Ps) :-
  \+ is_prime(P),
  P2 is P - 1,
  primes(Lower, Upper, P2, Ps).


% P40 (**) Goldbach's conjecture.
  % Goldbach's conjecture says that every positive even number greater than 2
  % is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the
  % most famous facts in number theory that has not been proved to be correct
  % in the general case. It has been numerically confirmed up to very large
  % numbers (much larger than we can go with our Prolog system). Write a
  % predicate to find the two prime numbers that sum up to a given even integer.
  %
  % Example:
  % ?- goldbach(28, L).
  % L = [5,23]

% from the answers section:
goldbach(4,[2,2]) :- !.
goldbach(N,L) :-
  N mod 2 =:= 0,
  N > 4,
  goldbach(N,L,3).

goldbach(N,[P,Q],P) :-
  Q is N - P,
  is_prime(Q), !.
goldbach(N,L,P) :-
  P < N,
  next_prime(P,P1),
  goldbach(N,L,P1).

next_prime(P,P1) :-
  P1 is P + 2,
  is_prime(P1),
  !.
next_prime(P,P1) :-
  P2 is P + 2,
  next_prime(P2,P1).


% P41 (**) A list of Goldbach compositions.
  % Given a range of integers by its lower and upper limit, print a list of all
  % even numbers and their Goldbach composition.
  % Example:
  % ?- goldbach_list(9,20).
  % 10 = 3 + 7
  % 12 = 5 + 7
  % 14 = 3 + 11
  % 16 = 3 + 13
  % 18 = 5 + 13
  % 20 = 3 + 17

  % In most cases, if an even number is written as the sum of two prime numbers,
  % one of them is very small. Very rarely, the primes are both bigger than say
  % 50. Try to find out how many such cases there are in the range 2..3000.

  % Example (for a print limit of 50):
  % ?- goldbach_list(1,2000,50).
  % 992 = 73 + 919
  % 1382 = 61 + 1321
  % 1856 = 67 + 1789
  % 1928 = 61 + 1867

goldbach_list(L, U) :-
  goldbach_list(L, U, 2).
goldbach_list(L, U, _) :- L > U, !.
goldbach_list(L, U, Limit) :-
  L mod 2 =\= 0,
  L1 is L + 1,
  goldbach_list(L1, U, Limit).
goldbach_list(L, U, Limit) :-
  L =< U,
  L mod 2 =:= 0,
  print_goldbach(L, Limit),
  L1 is L + 2,
  goldbach_list(L1, U, Limit).

print_goldbach(L, _) :-
  L < 4.
print_goldbach(L, Limit) :-
  goldbach(L, [A,_]),
  A < Limit.
print_goldbach(L, Limit) :-
  goldbach(L, [A,B]),
  A >= Limit,
  writef('%t = %t + %t\n', [L,A,B]).
