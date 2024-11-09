insert_at(X, [], _, [X]) :- !.
insert_at(X, L, 1, L1) :- append([X], L, L1), !.
insert_at(X, [Head | Tail], P, L1) :- P1 is P - 1, insert_at(X, Tail, P1, L2), append([Head], L2, L1).