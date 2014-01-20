% http://web.mit.edu/puzzle/www/2014/puzzle/time_flies/

a([buffalo]).
a([daddy]).
a([flying]).
a([gostak]).
a([had]).
a([james]).
a([john]).
a([planes]).
a([while]).

b([book]).
b([bring]).
b([colorless]).
b([did]).
b([dont]).
b([effect]).
b([i]).
b([that]).
b([what]).
b([you]).

c([every]).
c([farmer]).
c([of]).
c([on]).
c([out]).
c([saw]).
c([to]).
c([up]).
c([want]).
c([who]).

x([a]).
x([an]).
x([be]).
x([beats]).
x([better]).
x([binoculars]).
x([building]).
x([can]).
x([dangerous]).
x([distims]).
x([donkey]).
x([doshes]).
x([flag]).
x([for]).
x([furiously]).
x([hanging]).
x([it]).
x([man]).
x([owns]).
x([past]).
x([raced]).
x([read]).
x([teacher]).
x([the]).
x([was]).
x([with]).

y([fell]).
y([front]).
y([sleep]).

z([american]).
z([barn]).
z([green]).
z([horse]).
z([ideas]).
z([in]).


clauses(F1, F2, S) :-
  append(S1, S2, S),
  S1 \== [], S2 \== [],
  call(F1, S1), call(F2, S2).


% S → AP EP
s(S) :- clauses(ap, ep, S).
% S → BP
s(S) :- bp(S).
% S → CP UP
s(S) :- clauses(cp, up, S).
% S → DP EP
s(S) :- clauses(dp, ep, S).
% S → EP UP
s(S) :- clauses(ep, up, S).
% S → FP VP
s(S) :- clauses(fp, vp, S).
% S → GP XP
s(S) :- clauses(gp, xp, S).
% S → HP
s(S) :- hp(S).
% S → IP VP
s(S) :- clauses(ip, vp, S).
% S → JP IP
s(S) :- clauses(jp, ip, S).

% AP → WP YP
ap(S) :- clauses(wp, yp, S).

% BP → A
bp(S) :- a(S).
% BP → A BP
bp(S) :- clauses(a, bp, S).

% CP → B YP
cp(S) :- clauses(b, yp, S).

% DP → A IP
dp(S) :- clauses(a, ip, S).

% EP → C EP
ep(S) :- clauses(c, ep, S).
% EP → C XP
ep(S) :- clauses(c, xp, S).

% FP → A JP
fp(S) :- clauses(a, jp, S).

% GP → UP A
gp(S) :- clauses(up, a, S).

% HP → UP YP
hp(S) :- clauses(up, yp, S).

% IP → B EP
ip(S) :- clauses(b, ep, S).
% IP → B IP
ip(S) :- clauses(b, ip, S).

% JP → A JP
jp(S) :- clauses(a, jp, S).
% JP → UP UP
jp(S) :- clauses(up, up, S).

% UP → VP
up(S) :- vp(S).
% UP → X
up(S) :- x(S).

% VP → WP
vp(S) :- wp(S).
% VP → X
vp(S) :- x(S).

% WP → X
wp(S) :- x(S).

% XP → X
xp(S) :- x(S).
% XP → XP XP
xp(S) :- clauses(xp, xp, S).

% YP → X YP
yp(S) :- clauses(x, yp, S).
% YP → YP Y
yp(S) :- clauses(yp, y, S).
% YP → ZP
yp(S) :- zp(S).

% ZP → Z
zp(S) :- z(S).
% ZP → Z YP
zp(S) :- clauses(z, yp, S).
% ZP → ZP Y
zp(S) :- clauses(zp, y, S).


count_parse_trees([L1,L2,L3,L4,L5,L6,L7,L8,L9,L10]) :-
  aggregate_all(count, s([an, american, flag, was, hanging, in, front, of, every, building]), L1),
  aggregate_all(count, s([buffalo, buffalo, buffalo, buffalo, buffalo, buffalo, buffalo, buffalo]), L2),
  aggregate_all(count, s([colorless, green, ideas, sleep, furiously]), L3),
  aggregate_all(count, s([daddy, what, did, you, bring, that, book, that, i, dont, want, to, be, read, to, out, of, up, for]), L4),
  aggregate_all(count, s([every, farmer, who, owns, a, donkey, beats, it]), L5),
  aggregate_all(count, s([flying, planes, can, be, dangerous]), L6),
  aggregate_all(count, s([the, gostak, distims, the, doshes]), L7),
  aggregate_all(count, s([the, horse, raced, past, the, barn, fell]), L8),
  aggregate_all(count, s([i, saw, the, man, with, the, binoculars]), L9),
  aggregate_all(count, s([john, while, james, had, had, had, had, had, had, had, had, had, had, had, a, better, effect, on, the, teacher]), L10).

% ?- count_parse_trees(Counts).
% Counts = [7, 1, 12, 1, 15, 18, 6, 21, 10, 9].

% GALAORFUJI
% APPLE
