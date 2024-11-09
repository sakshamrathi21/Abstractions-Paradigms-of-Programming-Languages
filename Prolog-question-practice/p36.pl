is_prime_iter(X, N) :- X < N*N, !.
is_prime_iter(X, N) :- X mod N =\= 0,
                       L is N + 1,
                       is_prime_iter(X, L).
is_prime(X) :- X > 1,
               is_prime_iter(X, 2).

prime_factor_iter(N, I, []) :- I > N, !.    
prime_factor_iter(N, I, L) :- N mod I =:= 0, is_prime(I), X is N//I, prime_factor_iter(X, I, L1), append([I], L1, L), !.
prime_factor_iter(N, I, L) :- M is I + 1, prime_factor_iter(N, M, L).
prime_factor_list(N, L) :- prime_factor_iter(N, 2, L).

run_length_encode([], []).
run_length_encode([X], [[X, 1]]) :- !.
run_length_encode([X, X | Rest], [[X, Count] | Encodedrest]) :-
    run_length_encode([X | Rest], [[X, Count1] | Encodedrest]),
    Count is Count1 + 1, !.
run_length_encode([X, Y | Rest], [[X, 1] | Encodedrest]) :-
    X =\= Y,
    run_length_encode([ Y | Rest], Encodedrest).

prime_factor_count_list(N, LM) :- prime_factor_list(N, L), run_length_encode(L, LM). 