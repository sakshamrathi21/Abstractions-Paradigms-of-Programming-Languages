moves(0, _, _, _, []) :- !.
moves(N, A, B, C, L) :-  N > 0, M is N - 1, moves(M, A, C, B, L1), moves(M, C, B, A, L3), L2 = [to(A, B)], append(L1, L2, L4), append(L4, L3, L).