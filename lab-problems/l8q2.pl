split([], 0, [], []) :- !.
split(L, 0, [], L) :- !.
split([H | T], N, [H | T1], L2) :- M is N - 1, split(T, M, T1, L2).