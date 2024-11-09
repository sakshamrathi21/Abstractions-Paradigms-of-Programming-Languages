oddSum([], 0) :- !.
oddSum([X], S) :- X mod 2 =:= 1, S is X, !.
oddSum([X], S) :- X mod 2 =:= 0, S is 0, !.
oddSum([Head | Tail], S) :- Head mod 2 =:= 1, oddSum(Tail, S1), S is S1 + Head, !.
oddSum([Head | Tail], S) :- Head mod 2 =:= 0, oddSum(Tail, S), !.