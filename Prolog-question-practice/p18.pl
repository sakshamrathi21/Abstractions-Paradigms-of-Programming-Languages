slice([], _, _, []) :- !.
slice(_, M, N, []) :- M > N, !.
slice([Head | Tail], 1, N, L1) :- Y is N - 1, slice(Tail, 1, Y, L2), append([Head], L2, L1), !.
slice([_ | Tail], M, N, L1) :- X is M - 1, Y is N - 1, slice(Tail, X, Y, L1).