element_at(nil, [], _) :- !.
element_at(H, [H | _], 1) :- !.
element_at(X, [_ | T], K) :- K > 0,
                             M is K - 1,
                             element_at(X, T, M).