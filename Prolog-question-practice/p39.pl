is_prime_iter(X, N) :- X < N*N, !.
is_prime_iter(X, N) :- X mod N =\= 0,
                       L is N + 1,
                       is_prime_iter(X, L).
is_prime(X) :- X > 1,
               is_prime_iter(X, 2).

list_prime(A, B, []) :- A > B, !.
list_prime(A, B, L) :- is_prime(A), M is A + 1, list_prime(M, B, L1), append([A], L1, L), !.
list_prime(A, B, L) :- not(is_prime(A)), M is A + 1, list_prime(M, B, L).
