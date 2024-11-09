reverse([],[]) :- !.
reverse([H | T], L1) :- reverse(T,T1), 
                        append(T1, [H], L1).