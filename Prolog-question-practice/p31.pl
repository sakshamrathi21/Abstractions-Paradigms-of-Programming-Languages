is_prime_iter(X, N) :- X < N*N, !.
is_prime_iter(X, N) :- X mod N =\= 0,
                       L is N + 1,
                       is_prime_iter(X, L).
is_prime(X) :- X > 1,
               is_prime_iter(X, 2).