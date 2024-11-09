gcd(X, 1, 1) :- X > 0, !.
gcd(X, Y, G) :- Y > X, gcd(Y, X, G), !.
gcd(X, Y, Y) :- X mod Y =:= 0, !.
gcd(X, Y, G) :- M is X mod Y, gcd(Y, M, G), !.