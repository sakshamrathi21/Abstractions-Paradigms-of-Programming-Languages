append([], X, X).
append([_ | Tail], Y, [_ | Z]) :- append(Tail, Y, Z).
prefix(X, Z) :- append(X, _, Z).
suffix(X, Z) :- append(_, X, Z).
member(X, [X | _]).
member(X, [_ | Y]) :- member(X, Y).
factorial(N, 1) :- N = 0.
factorial(N, Result) :- N > 0,
                        M is N - 1,
                        factorial(M, Subres),
                        Result is Subres*N.
max(X, Y, Y) :- X =< Y.
max(X, Y, X) :- X > Y.
